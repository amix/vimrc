" Author: Ben Falconer <ben@falconers.me.uk>, jtalowell <jtalowell@protonmail.com>
" Description: A language server for C

call ale#Set('c_cquery_executable', 'cquery')
call ale#Set('c_cquery_cache_directory', expand('~/.cache/cquery'))

function! ale_linters#c#cquery#GetProjectRoot(buffer) abort
    " Try to find cquery configuration files first.
    let l:config = ale#path#FindNearestFile(a:buffer, '.cquery')

    if !empty(l:config)
        return fnamemodify(l:config, ':h')
    endif

    " Fall back on default project root detection.
    return ale#c#FindProjectRoot(a:buffer)
endfunction

function! ale_linters#c#cquery#GetInitializationOptions(buffer) abort
    return {'cacheDirectory': ale#Var(a:buffer, 'c_cquery_cache_directory')}
endfunction

call ale#linter#Define('c', {
\   'name': 'cquery',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'c_cquery_executable')},
\   'command': '%e',
\   'project_root': function('ale_linters#c#cquery#GetProjectRoot'),
\   'initialization_options': function('ale_linters#c#cquery#GetInitializationOptions'),
\})
