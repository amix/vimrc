" Author: medains <https://github.com/medains>, ardis <https://github.com/ardisdreelath>
" Description: phpstan for PHP files

" Set to change the ruleset
let g:ale_php_phpstan_executable = get(g:, 'ale_php_phpstan_executable', 'phpstan')
let g:ale_php_phpstan_level = get(g:, 'ale_php_phpstan_level', '')
let g:ale_php_phpstan_configuration = get(g:, 'ale_php_phpstan_configuration', '')
let g:ale_php_phpstan_autoload = get(g:, 'ale_php_phpstan_autoload', '')

function! ale_linters#php#phpstan#GetCommand(buffer, version) abort
    let l:configuration = ale#Var(a:buffer, 'php_phpstan_configuration')
    let l:configuration_option = !empty(l:configuration)
    \   ? ' -c ' . ale#Escape(l:configuration)
    \   : ''

    let l:autoload = ale#Var(a:buffer, 'php_phpstan_autoload')
    let l:autoload_option = !empty(l:autoload)
    \   ? ' -a ' . ale#Escape(l:autoload)
    \   : ''

    let l:level =  ale#Var(a:buffer, 'php_phpstan_level')
    let l:config_file_exists = ale#path#FindNearestFile(a:buffer, 'phpstan.neon')

    if empty(l:level) && empty(l:config_file_exists)
        " if no configuration file is found, then use 4 as a default level
        let l:level = '4'
    endif

    let l:level_option = !empty(l:level)
    \   ? ' -l ' . ale#Escape(l:level)
    \   : ''

    let l:error_format = ale#semver#GTE(a:version, [0, 10, 3])
    \   ? ' --error-format raw'
    \   : ' --errorFormat raw'

    return '%e analyze --no-progress'
    \   . l:error_format
    \   . l:configuration_option
    \   . l:autoload_option
    \   . l:level_option
    \   . ' %s'
endfunction

function! ale_linters#php#phpstan#Handle(buffer, lines) abort
    " Matches against lines like the following:
    "
    " filename.php:15:message
    " C:\folder\filename.php:15:message
    let l:pattern = '^\([a-zA-Z]:\)\?[^:]\+:\(\d\+\):\(.*\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[2] + 0,
        \   'text': l:match[3],
        \   'type': 'E',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('php', {
\   'name': 'phpstan',
\   'executable': {b -> ale#Var(b, 'php_phpstan_executable')},
\   'command': {buffer -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale#Var(buffer, 'php_phpstan_executable'),
\       '%e --version',
\       function('ale_linters#php#phpstan#GetCommand'),
\   )},
\   'callback': 'ale_linters#php#phpstan#Handle',
\})
