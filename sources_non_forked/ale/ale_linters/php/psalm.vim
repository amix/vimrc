" Author: Matt Brown <https://github.com/muglug>
" Description: plugin for Psalm, static analyzer for PHP

call ale#Set('php_psalm_executable', 'psalm')
call ale#Set('php_psalm_options', '')
call ale#Set('php_psalm_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#php#psalm#GetProjectRoot(buffer) abort
    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

function! ale_linters#php#psalm#GetCommand(buffer) abort
    return '%e --language-server' . ale#Pad(ale#Var(a:buffer, 'php_psalm_options'))
endfunction

call ale#linter#Define('php', {
\   'name': 'psalm',
\   'lsp': 'stdio',
\   'executable': {b -> ale#node#FindExecutable(b, 'php_psalm', [
\       'vendor/bin/psalm',
\   ])},
\   'command': function('ale_linters#php#psalm#GetCommand'),
\   'project_root': function('ale_linters#php#psalm#GetProjectRoot'),
\})
