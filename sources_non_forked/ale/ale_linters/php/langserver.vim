" Author: Eric Stern <eric@ericstern.com>
" Description: PHP Language server integration for ALE

call ale#Set('php_langserver_executable', 'php-language-server.php')
call ale#Set('php_langserver_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#php#langserver#GetProjectRoot(buffer) abort
    let l:composer_path = ale#path#FindNearestFile(a:buffer, 'composer.json')

    if (!empty(l:composer_path))
        return fnamemodify(l:composer_path, ':h')
    endif

    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

call ale#linter#Define('php', {
\   'name': 'langserver',
\   'lsp': 'stdio',
\   'executable': {b -> ale#path#FindExecutable(b, 'php_langserver', [
\       'vendor/bin/php-language-server.php',
\   ])},
\   'command': 'php %e',
\   'project_root': function('ale_linters#php#langserver#GetProjectRoot'),
\})
