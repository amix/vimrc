" Author: Eric Stern <eric@ericstern.com>
" Description: PHP Language server integration for ALE

call ale#Set('php_langserver_executable', 'php-language-server.php')
call ale#Set('php_langserver_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#php#langserver#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'php_langserver', [
    \   'vendor/bin/php-language-server.php',
    \])
endfunction

function! ale_linters#php#langserver#GetCommand(buffer) abort
    return 'php ' . ale#Escape(ale_linters#php#langserver#GetExecutable(a:buffer))
endfunction

function! ale_linters#php#langserver#GetProjectRoot(buffer) abort
    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

call ale#linter#Define('php', {
\   'name': 'langserver',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#php#langserver#GetExecutable',
\   'command_callback': 'ale_linters#php#langserver#GetCommand',
\   'language': 'php',
\   'project_root_callback': 'ale_linters#php#langserver#GetProjectRoot',
\})
