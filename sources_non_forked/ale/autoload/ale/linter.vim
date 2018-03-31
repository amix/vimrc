call ale#Set('wrap_command_as_one_argument', 0)
" Author: w0rp <devw0rp@gmail.com>
" Description: Linter registration and lazy-loading
"   Retrieves linters as requested by the engine, loading them if needed.

let s:linters = {}

" Default filetype aliases.
" The user defined aliases will be merged with this Dictionary.
let s:default_ale_linter_aliases = {
\   'Dockerfile': 'dockerfile',
\   'csh': 'sh',
\   'plaintex': 'tex',
\   'systemverilog': 'verilog',
\   'zsh': 'sh',
\}

" Default linters to run for particular filetypes.
" The user defined linter selections will be merged with this Dictionary.
"
" No linters are used for plaintext files by default.
"
" Only cargo is enabled for Rust by default.
" rpmlint is disabled by default because it can result in code execution.
"
" NOTE: Update the g:ale_linters documentation when modifying this.
let s:default_ale_linters = {
\   'csh': ['shell'],
\   'go': ['gofmt', 'golint', 'go vet'],
\   'help': [],
\   'perl': ['perlcritic'],
\   'python': ['flake8', 'mypy', 'pylint'],
\   'rust': ['cargo'],
\   'spec': [],
\   'text': [],
\   'zsh': ['shell'],
\}

" Testing/debugging helper to unload all linters.
function! ale#linter#Reset() abort
    let s:linters = {}
endfunction

function! s:IsCallback(value) abort
    return type(a:value) == type('') || type(a:value) == type(function('type'))
endfunction

function! s:IsBoolean(value) abort
    return type(a:value) == type(0) && (a:value == 0 || a:value == 1)
endfunction

function! ale#linter#PreProcess(linter) abort
    if type(a:linter) != type({})
        throw 'The linter object must be a Dictionary'
    endif

    let l:obj = {
    \   'add_newline': get(a:linter, 'add_newline', 0),
    \   'name': get(a:linter, 'name'),
    \   'lsp': get(a:linter, 'lsp', ''),
    \}

    if type(l:obj.name) != type('')
        throw '`name` must be defined to name the linter'
    endif

    let l:needs_address = l:obj.lsp is# 'socket'
    let l:needs_executable = l:obj.lsp isnot# 'socket'
    let l:needs_command = l:obj.lsp isnot# 'socket'
    let l:needs_lsp_details = !empty(l:obj.lsp)

    if empty(l:obj.lsp)
        let l:obj.callback = get(a:linter, 'callback')

        if !s:IsCallback(l:obj.callback)
            throw '`callback` must be defined with a callback to accept output'
        endif
    endif

    if index(['', 'socket', 'stdio', 'tsserver'], l:obj.lsp) < 0
        throw '`lsp` must be either `''lsp''` or `''tsserver''` if defined'
    endif

    if !l:needs_executable
        if has_key(a:linter, 'executable')
        \|| has_key(a:linter, 'executable_callback')
            throw '`executable` and `executable_callback` cannot be used when lsp == ''socket'''
        endif
    elseif has_key(a:linter, 'executable_callback')
        let l:obj.executable_callback = a:linter.executable_callback

        if !s:IsCallback(l:obj.executable_callback)
            throw '`executable_callback` must be a callback if defined'
        endif
    elseif has_key(a:linter, 'executable')
        let l:obj.executable = a:linter.executable

        if type(l:obj.executable) != type('')
            throw '`executable` must be a string if defined'
        endif
    else
        throw 'Either `executable` or `executable_callback` must be defined'
    endif

    if !l:needs_command
        if has_key(a:linter, 'command')
        \|| has_key(a:linter, 'command_callback')
        \|| has_key(a:linter, 'command_chain')
            throw '`command` and `command_callback` and `command_chain` cannot be used when lsp == ''socket'''
        endif
    elseif has_key(a:linter, 'command_chain')
        let l:obj.command_chain = a:linter.command_chain

        if type(l:obj.command_chain) != type([])
            throw '`command_chain` must be a List'
        endif

        if empty(l:obj.command_chain)
            throw '`command_chain` must contain at least one item'
        endif

        let l:link_index = 0

        for l:link in l:obj.command_chain
            let l:err_prefix = 'The `command_chain` item ' . l:link_index . ' '

            if !s:IsCallback(get(l:link, 'callback'))
                throw l:err_prefix . 'must define a `callback` function'
            endif

            if has_key(l:link, 'output_stream')
                if type(l:link.output_stream) != type('')
                \|| index(['stdout', 'stderr', 'both'], l:link.output_stream) < 0
                    throw l:err_prefix . '`output_stream` flag must be '
                    \   . "'stdout', 'stderr', or 'both'"
                endif
            endif

            if has_key(l:link, 'read_buffer') && !s:IsBoolean(l:link.read_buffer)
                throw l:err_prefix . 'value for `read_buffer` must be `0` or `1`'
            endif

            let l:link_index += 1
        endfor
    elseif has_key(a:linter, 'command_callback')
        let l:obj.command_callback = a:linter.command_callback

        if !s:IsCallback(l:obj.command_callback)
            throw '`command_callback` must be a callback if defined'
        endif
    elseif has_key(a:linter, 'command')
        let l:obj.command = a:linter.command

        if type(l:obj.command) != type('')
            throw '`command` must be a string if defined'
        endif
    else
        throw 'Either `command`, `executable_callback`, `command_chain` '
        \   . 'must be defined'
    endif

    if (
    \   has_key(a:linter, 'command')
    \   + has_key(a:linter, 'command_chain')
    \   + has_key(a:linter, 'command_callback')
    \) > 1
        throw 'Only one of `command`, `command_callback`, or `command_chain` '
        \   . 'should be set'
    endif

    if !l:needs_address
        if has_key(a:linter, 'address_callback')
            throw '`address_callback` cannot be used when lsp != ''socket'''
        endif
    elseif has_key(a:linter, 'address_callback')
        let l:obj.address_callback = a:linter.address_callback

        if !s:IsCallback(l:obj.address_callback)
            throw '`address_callback` must be a callback if defined'
        endif
    else
        throw '`address_callback` must be defined for getting the LSP address'
    endif

    if l:needs_lsp_details
        let l:obj.language_callback = get(a:linter, 'language_callback')

        if !s:IsCallback(l:obj.language_callback)
            throw '`language_callback` must be a callback for LSP linters'
        endif

        let l:obj.project_root_callback = get(a:linter, 'project_root_callback')

        if !s:IsCallback(l:obj.project_root_callback)
            throw '`project_root_callback` must be a callback for LSP linters'
        endif
    endif

    let l:obj.output_stream = get(a:linter, 'output_stream', 'stdout')

    if type(l:obj.output_stream) != type('')
    \|| index(['stdout', 'stderr', 'both'], l:obj.output_stream) < 0
        throw "`output_stream` must be 'stdout', 'stderr', or 'both'"
    endif

    " An option indicating that this linter should only be run against the
    " file on disk.
    let l:obj.lint_file = get(a:linter, 'lint_file', 0)

    if !s:IsBoolean(l:obj.lint_file)
        throw '`lint_file` must be `0` or `1`'
    endif

    " An option indicating that the buffer should be read.
    let l:obj.read_buffer = get(a:linter, 'read_buffer', !l:obj.lint_file)

    if !s:IsBoolean(l:obj.read_buffer)
        throw '`read_buffer` must be `0` or `1`'
    endif

    if l:obj.lint_file && l:obj.read_buffer
        throw 'Only one of `lint_file` or `read_buffer` can be `1`'
    endif

    let l:obj.aliases = get(a:linter, 'aliases', [])

    if type(l:obj.aliases) != type([])
    \|| len(filter(copy(l:obj.aliases), 'type(v:val) != type('''')')) > 0
        throw '`aliases` must be a List of String values'
    endif

    return l:obj
endfunction

function! ale#linter#Define(filetype, linter) abort
    if !has_key(s:linters, a:filetype)
        let s:linters[a:filetype] = []
    endif

    let l:new_linter = ale#linter#PreProcess(a:linter)

    call add(s:linters[a:filetype], l:new_linter)
endfunction

function! ale#linter#GetAll(filetypes) abort
    let l:combined_linters = []

    for l:filetype in a:filetypes
        " Load linter defintions from files if we haven't loaded them yet.
        if !has_key(s:linters, l:filetype)
            execute 'silent! runtime! ale_linters/' . l:filetype . '/*.vim'

            " Always set an empty List for the loaded linters if we don't find
            " any. This will prevent us from executing the runtime command
            " many times, redundantly.
            if !has_key(s:linters, l:filetype)
                let s:linters[l:filetype] = []
            endif
        endif

        call extend(l:combined_linters, get(s:linters, l:filetype, []))
    endfor

    return l:combined_linters
endfunction

function! s:GetAliasedFiletype(original_filetype) abort
    let l:buffer_aliases = get(b:, 'ale_linter_aliases', {})

    " b:ale_linter_aliases can be set to a List.
    if type(l:buffer_aliases) is type([])
        return l:buffer_aliases
    endif

    " Check for aliased filetypes first in a buffer variable,
    " then the global variable,
    " then in the default mapping,
    " otherwise use the original filetype.
    for l:dict in [
    \   l:buffer_aliases,
    \   g:ale_linter_aliases,
    \   s:default_ale_linter_aliases,
    \]
        if has_key(l:dict, a:original_filetype)
            return l:dict[a:original_filetype]
        endif
    endfor

    return a:original_filetype
endfunction

function! ale#linter#ResolveFiletype(original_filetype) abort
    let l:filetype = s:GetAliasedFiletype(a:original_filetype)

    if type(l:filetype) != type([])
        return [l:filetype]
    endif

    return l:filetype
endfunction

function! s:GetLinterNames(original_filetype) abort
    let l:buffer_ale_linters = get(b:, 'ale_linters', {})

    " b:ale_linters can be set to 'all'
    if l:buffer_ale_linters is# 'all'
        return 'all'
    endif

    " b:ale_linters can be set to a List.
    if type(l:buffer_ale_linters) is type([])
        return l:buffer_ale_linters
    endif

    " Try to get a buffer-local setting for the filetype
    if has_key(l:buffer_ale_linters, a:original_filetype)
        return l:buffer_ale_linters[a:original_filetype]
    endif

    " Try to get a global setting for the filetype
    if has_key(g:ale_linters, a:original_filetype)
        return g:ale_linters[a:original_filetype]
    endif

    " If the user has configured ALE to only enable linters explicitly, then
    " don't enable any linters by default.
    if g:ale_linters_explicit
        return []
    endif

    " Try to get a default setting for the filetype
    if has_key(s:default_ale_linters, a:original_filetype)
        return s:default_ale_linters[a:original_filetype]
    endif

    return 'all'
endfunction

function! ale#linter#Get(original_filetypes) abort
    let l:possibly_duplicated_linters = []

    " Handle dot-separated filetypes.
    for l:original_filetype in split(a:original_filetypes, '\.')
        let l:filetype = ale#linter#ResolveFiletype(l:original_filetype)
        let l:linter_names = s:GetLinterNames(l:original_filetype)
        let l:all_linters = ale#linter#GetAll(l:filetype)
        let l:filetype_linters = []

        if type(l:linter_names) == type('') && l:linter_names is# 'all'
            let l:filetype_linters = l:all_linters
        elseif type(l:linter_names) == type([])
            " Select only the linters we or the user has specified.
            for l:linter in l:all_linters
                let l:name_list = [l:linter.name] + l:linter.aliases

                for l:name in l:name_list
                    if index(l:linter_names, l:name) >= 0
                        call add(l:filetype_linters, l:linter)
                        break
                    endif
                endfor
            endfor
        endif

        call extend(l:possibly_duplicated_linters, l:filetype_linters)
    endfor

    let l:name_list = []
    let l:combined_linters = []

    " Make sure we override linters so we don't get two with the same name,
    " like 'eslint' for both 'javascript' and 'typescript'
    "
    " Note that the reverse calls here modify the List variables.
    for l:linter in reverse(l:possibly_duplicated_linters)
        if index(l:name_list, l:linter.name) < 0
            call add(l:name_list, l:linter.name)
            call add(l:combined_linters, l:linter)
        endif
    endfor

    return reverse(l:combined_linters)
endfunction

" Given a buffer and linter, get the executable String for the linter.
function! ale#linter#GetExecutable(buffer, linter) abort
    return has_key(a:linter, 'executable_callback')
    \   ? ale#util#GetFunction(a:linter.executable_callback)(a:buffer)
    \   : a:linter.executable
endfunction

" Given a buffer and linter, get the command String for the linter.
" The command_chain key is not supported.
function! ale#linter#GetCommand(buffer, linter) abort
    return has_key(a:linter, 'command_callback')
    \   ? ale#util#GetFunction(a:linter.command_callback)(a:buffer)
    \   : a:linter.command
endfunction

" Given a buffer and linter, get the address for connecting to the server.
function! ale#linter#GetAddress(buffer, linter) abort
    return has_key(a:linter, 'address_callback')
    \   ? ale#util#GetFunction(a:linter.address_callback)(a:buffer)
    \   : a:linter.address
endfunction

" Given a buffer, an LSP linter, and a callback to register for handling
" messages, start up an LSP linter and get ready to receive errors or
" completions.
function! ale#linter#StartLSP(buffer, linter, callback) abort
    let l:command = ''
    let l:address = ''
    let l:root = ale#util#GetFunction(a:linter.project_root_callback)(a:buffer)

    if empty(l:root) && a:linter.lsp isnot# 'tsserver'
        " If there's no project root, then we can't check files with LSP,
        " unless we are using tsserver, which doesn't use project roots.
        return {}
    endif

    if a:linter.lsp is# 'socket'
        let l:address = ale#linter#GetAddress(a:buffer, a:linter)
        let l:conn_id = ale#lsp#ConnectToAddress(
        \   l:address,
        \   l:root,
        \   a:callback,
        \)
    else
        let l:executable = ale#linter#GetExecutable(a:buffer, a:linter)

        if !executable(l:executable)
            return {}
        endif

        let l:command = ale#job#PrepareCommand(
        \   a:buffer,
        \   ale#linter#GetCommand(a:buffer, a:linter),
        \)
        let l:conn_id = ale#lsp#StartProgram(
        \   l:executable,
        \   l:command,
        \   l:root,
        \   a:callback,
        \)
    endif

    let l:language_id = ale#util#GetFunction(a:linter.language_callback)(a:buffer)

    if !l:conn_id
        if g:ale_history_enabled && !empty(l:command)
            call ale#history#Add(a:buffer, 'failed', l:conn_id, l:command)
        endif

        return {}
    endif

    if ale#lsp#OpenDocumentIfNeeded(l:conn_id, a:buffer, l:root, l:language_id)
        if g:ale_history_enabled && !empty(l:command)
            call ale#history#Add(a:buffer, 'started', l:conn_id, l:command)
        endif
    endif

    " The change message needs to be sent for tsserver before doing anything.
    if a:linter.lsp is# 'tsserver'
        call ale#lsp#Send(l:conn_id, ale#lsp#tsserver_message#Change(a:buffer))
    endif

    return {
    \   'connection_id': l:conn_id,
    \   'command': l:command,
    \   'project_root': l:root,
    \   'language_id': l:language_id,
    \}
endfunction
