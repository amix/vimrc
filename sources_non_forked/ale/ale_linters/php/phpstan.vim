" Author: medains <https://github.com/medains>, ardis <https://github.com/ardisdreelath>
" Description: phpstan for PHP files

" Set to change the ruleset
let g:ale_php_phpstan_executable = get(g:, 'ale_php_phpstan_executable', 'phpstan')
let g:ale_php_phpstan_level = get(g:, 'ale_php_phpstan_level', '4')
let g:ale_php_phpstan_configuration = get(g:, 'ale_php_phpstan_configuration', '')

function! ale_linters#php#phpstan#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'php_phpstan_executable')
endfunction

function! ale_linters#php#phpstan#VersionCheck(buffer) abort
    let l:executable = ale_linters#php#phpstan#GetExecutable(a:buffer)

    " If we have previously stored the version number in a cache, then
    " don't look it up again.
    if ale#semver#HasVersion(l:executable)
        " Returning an empty string skips this command.
        return ''
    endif

    let l:executable = ale#Escape(l:executable)

    return l:executable . ' --version'
endfunction

function! ale_linters#php#phpstan#GetCommand(buffer, version_output) abort
    let l:configuration = ale#Var(a:buffer, 'php_phpstan_configuration')
    let l:configuration_option = !empty(l:configuration)
    \   ? ' -c ' . l:configuration
    \   : ''

    let l:executable = ale_linters#php#phpstan#GetExecutable(a:buffer)
    let l:version = ale#semver#GetVersion(l:executable, a:version_output)
    let l:error_format = ale#semver#GTE(l:version, [0, 10, 3])
    \   ? ' --error-format raw'
    \   : ' --errorFormat raw'

    return '%e analyze -l'
    \   . ale#Var(a:buffer, 'php_phpstan_level')
    \   . l:error_format
    \   . l:configuration_option
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
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('php', {
\   'name': 'phpstan',
\   'executable_callback': 'ale_linters#php#phpstan#GetExecutable',
\   'command_chain': [
\       {'callback': 'ale_linters#php#phpstan#VersionCheck'},
\       {'callback': 'ale_linters#php#phpstan#GetCommand'},
\   ],
\   'callback': 'ale_linters#php#phpstan#Handle',
\})
