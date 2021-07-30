" Author: medains <https://github.com/medains>, ardis <https://github.com/ardisdreelath>
" Description: phpstan for PHP files

" Set to change the ruleset
let g:ale_php_phpstan_executable = get(g:, 'ale_php_phpstan_executable', 'phpstan')
let g:ale_php_phpstan_level = get(g:, 'ale_php_phpstan_level', '')
let g:ale_php_phpstan_configuration = get(g:, 'ale_php_phpstan_configuration', '')
let g:ale_php_phpstan_autoload = get(g:, 'ale_php_phpstan_autoload', '')
call ale#Set('php_phpstan_use_global', get(g:, 'ale_use_global_executables', 0))

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
    let l:dist_config_file_exists = ale#path#FindNearestFile(a:buffer, 'phpstan.neon.dist')

    if empty(l:level) && empty(l:config_file_exists) && empty(l:dist_config_file_exists)
        " if no configuration file is found, then use 4 as a default level
        let l:level = '4'
    endif

    let l:level_option = !empty(l:level)
    \   ? ' -l ' . ale#Escape(l:level)
    \   : ''

    let l:error_format = ale#semver#GTE(a:version, [0, 10, 3])
    \   ? ' --error-format json'
    \   : ' --errorFormat json'

    return '%e analyze --no-progress'
    \   . l:error_format
    \   . l:configuration_option
    \   . l:autoload_option
    \   . l:level_option
    \   . ' %s'
endfunction

function! ale_linters#php#phpstan#Handle(buffer, lines) abort
    let l:res = ale#util#FuzzyJSONDecode(a:lines, {'files': []})
    let l:output = []

    if type(l:res.files) is v:t_list
        return l:output
    endif

    for l:err in l:res.files[expand('#' . a:buffer .':p')].messages
        call add(l:output, {
        \   'lnum': l:err.line,
        \   'text': l:err.message,
        \   'type': 'E',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('php', {
\   'name': 'phpstan',
\   'executable': {buffer -> ale#path#FindExecutable(buffer, 'php_phpstan', [
\       'vendor/bin/phpstan',
\       'phpstan'
\   ])},
\   'command': {buffer -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale#path#FindExecutable(buffer, 'php_phpstan', [
\           'vendor/bin/phpstan',
\           'phpstan'
\       ]),
\       '%e --version',
\       function('ale_linters#php#phpstan#GetCommand'),
\   )},
\   'callback': 'ale_linters#php#phpstan#Handle',
\})
