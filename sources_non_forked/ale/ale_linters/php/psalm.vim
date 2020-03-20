" Author: Matt Brown <https://github.com/muglug>
" Description: plugin for Psalm, static analyzer for PHP

call ale#Set('psalm_langserver_executable', 'psalm')
call ale#Set('psalm_langserver_options', '')
call ale#Set('psalm_langserver_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#php#psalm#GetProjectRoot(buffer) abort
    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

function! ale_linters#php#psalm#GetCommand(buffer) abort
    return '%e --language-server' . ale#Pad(ale#Var(a:buffer, 'psalm_langserver_options'))
endfunction

call ale#linter#Define('php', {
\   'name': 'psalm',
\   'lsp': 'stdio',
\   'executable': {b -> ale#node#FindExecutable(b, 'psalm_langserver', [
\       'vendor/bin/psalm',
\   ])},
\   'command': function('ale_linters#php#psalm#GetCommand'),
\   'project_root': function('ale_linters#php#psalm#GetProjectRoot'),
\})
