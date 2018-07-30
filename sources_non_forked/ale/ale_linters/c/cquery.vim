" Author: Ben Falconer <ben@falconers.me.uk>, jtalowell <jtalowell@protonmail.com>
" Description: A language server for C

call ale#Set('c_cquery_executable', 'cquery')
call ale#Set('c_cquery_cache_directory', expand('~/.cache/cquery'))

function! ale_linters#c#cquery#GetProjectRoot(buffer) abort
    let l:project_root = ale#path#FindNearestFile(a:buffer, 'compile_commands.json')
    return !empty(l:project_root) ? fnamemodify(l:project_root, ':h') : ''
endfunction

function! ale_linters#c#cquery#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'c_cquery_executable')
endfunction

function! ale_linters#c#cquery#GetCommand(buffer) abort
    let l:executable = ale_linters#c#cquery#GetExecutable(a:buffer)
    return ale#Escape(l:executable)
endfunction

function! ale_linters#c#cquery#GetInitializationOptions(buffer) abort
    return {'cacheDirectory': ale#Var(a:buffer, 'c_cquery_cache_directory')}
endfunction

call ale#linter#Define('c', {
\   'name': 'cquery',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#c#cquery#GetExecutable',
\   'command_callback': 'ale_linters#c#cquery#GetCommand',
\   'project_root_callback': 'ale_linters#c#cquery#GetProjectRoot',
\   'initialization_options_callback': 'ale_linters#c#cquery#GetInitializationOptions',
\})
