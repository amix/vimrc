let s:chain_results = []

function! ale#assert#WithChainResults(...) abort
    let s:chain_results = a:000
endfunction

function! s:GetLinter() abort
    let l:linters = ale#linter#GetLintersLoaded()
    let l:filetype_linters = get(values(l:linters), 0, [])

    if len(l:linters) is 0 || len(l:filetype_linters) is 0
        throw 'No linters were loaded'
    endif

    if len(l:linters) > 1 || len(l:filetype_linters) > 1
        throw 'More than one linter was loaded'
    endif

    return l:filetype_linters[0]
endfunction

" Load the currently loaded linter for a test case, and check that the command
" matches the given string.
function! ale#assert#Linter(expected_executable, expected_command) abort
    let l:buffer = bufnr('')
    let l:linter = s:GetLinter()
    let l:executable = ale#linter#GetExecutable(l:buffer, l:linter)

    if has_key(l:linter, 'command_chain')
        let l:callbacks = map(copy(l:linter.command_chain), 'v:val.callback')

        " If the expected command is a string, just check the last one.
        if type(a:expected_command) is v:t_string
            if len(l:callbacks) is 1
                let l:command = call(l:callbacks[0], [l:buffer])
            else
                let l:input = get(s:chain_results, len(l:callbacks) - 2, [])
                let l:command = call(l:callbacks[-1], [l:buffer, l:input])
            endif
        else
            let l:command = []
            let l:chain_index = 0

            for l:Callback in l:callbacks
                if l:chain_index is 0
                    call add(l:command, call(l:Callback, [l:buffer]))
                else
                    let l:input = get(s:chain_results, l:chain_index - 1, [])
                    call add(l:command, call(l:Callback, [l:buffer, l:input]))
                endif

                let l:chain_index += 1
            endfor
        endif
    else
        let l:command = ale#linter#GetCommand(l:buffer, l:linter)
    endif

    if type(l:command) is v:t_string
        " Replace %e with the escaped executable, so tests keep passing after
        " linters are changed to use %e.
        let l:command = substitute(l:command, '%e', '\=ale#Escape(l:executable)', 'g')
    else
        call map(l:command, 'substitute(v:val, ''%e'', ''\=ale#Escape(l:executable)'', ''g'')')
    endif

    AssertEqual
    \   [a:expected_executable, a:expected_command],
    \   [l:executable, l:command]
endfunction

function! ale#assert#LinterNotExecuted() abort
    let l:buffer = bufnr('')
    let l:linter = s:GetLinter()
    let l:executable = ale#linter#GetExecutable(l:buffer, l:linter)

    Assert empty(l:executable), "The linter will be executed when it shouldn't be"
endfunction

function! ale#assert#LSPOptions(expected_options) abort
    let l:buffer = bufnr('')
    let l:linter = s:GetLinter()
    let l:initialization_options = ale#lsp_linter#GetOptions(l:buffer, l:linter)

    AssertEqual a:expected_options, l:initialization_options
endfunction

function! ale#assert#LSPConfig(expected_config) abort
    let l:buffer = bufnr('')
    let l:linter = s:GetLinter()
    let l:config = ale#lsp_linter#GetConfig(l:buffer, l:linter)

    AssertEqual a:expected_config, l:config
endfunction

function! ale#assert#LSPLanguage(expected_language) abort
    let l:buffer = bufnr('')
    let l:linter = s:GetLinter()
    let l:language = ale#util#GetFunction(l:linter.language_callback)(l:buffer)

    AssertEqual a:expected_language, l:language
endfunction

function! ale#assert#LSPProject(expected_root) abort
    let l:buffer = bufnr('')
    let l:linter = s:GetLinter()
    let l:root = ale#util#GetFunction(l:linter.project_root_callback)(l:buffer)

    AssertEqual a:expected_root, l:root
endfunction

function! ale#assert#LSPAddress(expected_address) abort
    let l:buffer = bufnr('')
    let l:linter = s:GetLinter()
    let l:address = ale#util#GetFunction(l:linter.address_callback)(l:buffer)

    AssertEqual a:expected_address, l:address
endfunction

" A dummy function for making sure this module is loaded.
function! ale#assert#SetUpLinterTest(filetype, name) abort
    " Set up a marker so ALE doesn't create real random temporary filenames.
    let g:ale_create_dummy_temporary_file = 1

    " Remove current linters.
    call ale#linter#Reset()
    call ale#linter#PreventLoading(a:filetype)

    let l:prefix = 'ale_' . a:filetype . '_' . a:name
    let b:filter_expr = 'v:val[: len(l:prefix) - 1] is# l:prefix'

    Save g:ale_c_build_dir
    unlet! g:ale_c_build_dir

    " Save and clear linter variables.
    " We'll load the runtime file to reset them to defaults.
    for l:key in filter(keys(g:), b:filter_expr)
        execute 'Save g:' . l:key
        unlet g:[l:key]
    endfor

    unlet! b:ale_c_build_dir

    for l:key in filter(keys(b:), b:filter_expr)
        unlet b:[l:key]
    endfor

    execute 'runtime ale_linters/' . a:filetype . '/' . a:name . '.vim'

    if !exists('g:dir')
        call ale#test#SetDirectory('/testplugin/test/command_callback')
    endif

    command! -nargs=+ WithChainResults :call ale#assert#WithChainResults(<args>)
    command! -nargs=+ AssertLinter :call ale#assert#Linter(<args>)
    command! -nargs=0 AssertLinterNotExecuted :call ale#assert#LinterNotExecuted()
    command! -nargs=+ AssertLSPOptions :call ale#assert#LSPOptions(<args>)
    command! -nargs=+ AssertLSPConfig :call ale#assert#LSPConfig(<args>)
    command! -nargs=+ AssertLSPLanguage :call ale#assert#LSPLanguage(<args>)
    command! -nargs=+ AssertLSPProject :call ale#assert#LSPProject(<args>)
    command! -nargs=+ AssertLSPAddress :call ale#assert#LSPAddress(<args>)
endfunction

function! ale#assert#TearDownLinterTest() abort
    unlet! g:ale_create_dummy_temporary_file
    let s:chain_results = []

    if exists(':WithChainResults')
        delcommand WithChainResults
    endif

    if exists(':AssertLinter')
        delcommand AssertLinter
    endif

    if exists(':AssertLinterNotExecuted')
        delcommand AssertLinterNotExecuted
    endif

    if exists(':AssertLSPOptions')
        delcommand AssertLSPOptions
    endif

    if exists(':AssertLSPConfig')
        delcommand AssertLSPConfig
    endif

    if exists(':AssertLSPLanguage')
        delcommand AssertLSPLanguage
    endif

    if exists(':AssertLSPProject')
        delcommand AssertLSPProject
    endif

    if exists(':AssertLSPAddress')
        delcommand AssertLSPAddress
    endif

    if exists('g:dir')
        call ale#test#RestoreDirectory()
    endif

    Restore

    call ale#linter#Reset()

    if exists('*ale#semver#ResetVersionCache')
        call ale#semver#ResetVersionCache()
    endif
endfunction
