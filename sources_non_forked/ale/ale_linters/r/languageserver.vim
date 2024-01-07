" Author: Eric Zhao <21zhaoe@protonmail.com>
" Author: ourigen <https://github.com/ourigen>
" Description: Implementation of the Language Server Protocol for R.

call ale#Set('r_languageserver_cmd', 'languageserver::run()')
call ale#Set('r_languageserver_config', {})

function! ale_linters#r#languageserver#GetCommand(buffer) abort
    let l:cmd_string = ale#Var(a:buffer, 'r_languageserver_cmd')

    return 'Rscript --no-save --no-restore --no-site-file --no-init-file -e ' . ale#Escape(l:cmd_string)
endfunction

function! ale_linters#r#languageserver#GetProjectRoot(buffer) abort
    let l:project_root = ale#path#FindNearestFile(a:buffer, '.Rprofile')

    return !empty(l:project_root) ? fnamemodify(l:project_root, ':h') : fnamemodify(a:buffer, ':h')
endfunction

call ale#linter#Define('r', {
\   'name': 'languageserver',
\   'aliases': ['r_language_server'],
\   'lsp': 'stdio',
\   'lsp_config': {b -> ale#Var(b, 'r_languageserver_config')},
\   'executable': 'Rscript',
\   'command': function('ale_linters#r#languageserver#GetCommand'),
\   'project_root': function('ale_linters#r#languageserver#GetProjectRoot')
\})
