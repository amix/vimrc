let s:command_output = []

function! ale#assert#GivenCommandOutput(...) abort
    let s:command_output = a:000
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

function! s:FormatExe(command, executable) abort
    return substitute(a:command, '%e', '\=ale#Escape(a:executable)', 'g')
endfunction

function! s:ProcessDeferredCommands(initial_result) abort
    let l:result = a:initial_result
    let l:command_index = 0
    let l:command = []

    while ale#command#IsDeferred(l:result)
        call add(l:command, s:FormatExe(l:result.command, l:result.executable))

        if get(g:, 'ale_run_synchronously_emulate_commands')
            " Don't run commands, but simulate the results.
            let l:Callback = g:ale_run_synchronously_callbacks[0]
            let l:output = get(s:command_output, l:command_index, [])
            call l:Callback(0, l:output)
            unlet g:ale_run_synchronously_callbacks

            let l:command_index += 1
        else
            " Run the commands in the shell, synchronously.
            call ale#test#FlushJobs()
        endif

        let l:result = l:result.value
    endwhile

    call add(l:command, l:result)

    return l:command
endfunction

" Load the currently loaded linter for a test case, and check that the command
" matches the given string.
function! ale#assert#Linter(expected_executable, expected_command) abort
    let l:buffer = bufnr('')
    let l:linter = s:GetLinter()
    let l:executable = ale#linter#GetExecutable(l:buffer, l:linter)

    while ale#command#IsDeferred(l:executable)
        call ale#test#FlushJobs()
        let l:executable = l:executable.value
    endwhile

    let l:command = s:ProcessDeferredCommands(
    \   ale#linter#GetCommand(l:buffer, l:linter),
    \)

    if type(a:expected_command) isnot v:t_list
        let l:command = l:command[-1]
    endif

    if type(l:command) is v:t_string
        " Replace %e with the escaped executable, so tests keep passing after
        " linters are changed to use %e.
        let l:command = s:FormatExe(l:command, l:executable)
    elseif type(l:command) is v:t_list
        call map(l:command, 's:FormatExe(v:val, l:executable)')
    endif

    AssertEqual
    \   [a:expected_executable, a:expected_command],
    \   [l:executable, l:command]
endfunction

function! ale#assert#Fixer(expected_result) abort
    let l:buffer = bufnr('')
    let l:result = s:ProcessDeferredCommands(s:FixerFunction(l:buffer))

    if type(a:expected_result) isnot v:t_list
        let l:result = l:result[-1]
    endif

    AssertEqual a:expected_result, l:result
endfunction

function! ale#assert#FixerNotExecuted() abort
    let l:buffer = bufnr('')
    let l:result = s:ProcessDeferredCommands(s:FixerFunction(l:buffer))[-1]

    Assert empty(l:result), "The fixer will be executed when it shouldn't be"
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
    let l:root = ale#lsp_linter#FindProjectRoot(l:buffer, l:linter)

    AssertEqual a:expected_root, l:root
endfunction

function! ale#assert#LSPAddress(expected_address) abort
    let l:buffer = bufnr('')
    let l:linter = s:GetLinter()
    let l:address = ale#linter#GetAddress(l:buffer, l:linter)

    AssertEqual a:expected_address, l:address
endfunction

function! ale#assert#SetUpLinterTestCommands() abort
    command! -nargs=+ GivenCommandOutput :call ale#assert#GivenCommandOutput(<args>)
    command! -nargs=+ AssertLinter :call ale#assert#Linter(<args>)
    command! -nargs=0 AssertLinterNotExecuted :call ale#assert#LinterNotExecuted()
    command! -nargs=+ AssertLSPOptions :call ale#assert#LSPOptions(<args>)
    command! -nargs=+ AssertLSPConfig :call ale#assert#LSPConfig(<args>)
    command! -nargs=+ AssertLSPLanguage :call ale#assert#LSPLanguage(<args>)
    command! -nargs=+ AssertLSPProject :call ale#assert#LSPProject(<args>)
    command! -nargs=+ AssertLSPAddress :call ale#assert#LSPAddress(<args>)
endfunction

function! ale#assert#SetUpFixerTestCommands() abort
    command! -nargs=+ GivenCommandOutput :call ale#assert#GivenCommandOutput(<args>)
    command! -nargs=+ AssertFixer :call ale#assert#Fixer(<args>)
    command! -nargs=0 AssertFixerNotExecuted :call ale#assert#FixerNotExecuted()
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

    Save g:ale_lsp_root
    let g:ale_lsp_root = {}

    Save b:ale_lsp_root
    unlet! b:ale_lsp_root

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

    call ale#assert#SetUpLinterTestCommands()

    let g:ale_run_synchronously = 1
    let g:ale_run_synchronously_emulate_commands = 1
endfunction

function! ale#assert#TearDownLinterTest() abort
    unlet! g:ale_create_dummy_temporary_file
    unlet! g:ale_run_synchronously
    unlet! g:ale_run_synchronously_callbacks
    unlet! g:ale_run_synchronously_emulate_commands
    unlet! g:ale_run_synchronously_command_results
    let s:command_output = []

    if exists(':GivenCommandOutput')
        delcommand GivenCommandOutput
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

function! ale#assert#SetUpFixerTest(filetype, name, ...) abort
    " If the suffix of the option names format is different, an additional
    " argument can be used for that instead.
    if a:0 > 1
        throw 'Too many arguments'
    endif

    " Set up a marker so ALE doesn't create real random temporary filenames.
    let g:ale_create_dummy_temporary_file = 1

    let l:function_name = ale#fix#registry#GetFunc(a:name)
    let s:FixerFunction = function(l:function_name)

    let l:option_suffix = get(a:000, 0, a:name)
    let l:prefix = 'ale_' . a:filetype . '_'
    \   . substitute(l:option_suffix, '-', '_', 'g')
    let b:filter_expr = 'v:val[: len(l:prefix) - 1] is# l:prefix'

    for l:key in filter(keys(g:), b:filter_expr)
        execute 'Save g:' . l:key
        unlet g:[l:key]
    endfor

    for l:key in filter(keys(b:), b:filter_expr)
        unlet b:[l:key]
    endfor

    execute 'runtime autoload/ale/fixers/' . substitute(a:name, '-', '_', 'g') . '.vim'

    if !exists('g:dir')
        call ale#test#SetDirectory('/testplugin/test/fixers')
    endif

    call ale#assert#SetUpFixerTestCommands()

    let g:ale_run_synchronously = 1
    let g:ale_run_synchronously_emulate_commands = 1
endfunction

function! ale#assert#TearDownFixerTest() abort
    unlet! g:ale_create_dummy_temporary_file
    unlet! g:ale_run_synchronously
    unlet! g:ale_run_synchronously_callbacks
    unlet! g:ale_run_synchronously_emulate_commands
    unlet! g:ale_run_synchronously_command_results
    let s:command_output = []
    unlet! s:FixerFunction

    if exists('g:dir')
        call ale#test#RestoreDirectory()
    endif

    Restore

    if exists('*ale#semver#ResetVersionCache')
        call ale#semver#ResetVersionCache()
    endif

    if exists(':GivenCommandOutput')
        delcommand GivenCommandOutput
    endif

    if exists(':AssertFixer')
        delcommand AssertFixer
    endif

    if exists(':AssertFixerNotExecuted')
        delcommand AssertFixerNotExecuted
    endif
endfunction
