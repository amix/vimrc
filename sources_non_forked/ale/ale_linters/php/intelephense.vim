" Author: Eric Stern <eric@ericstern.com>,
"         Arnold Chand <creativenull@outlook.com>
" Description: Intelephense language server integration for ALE

call ale#Set('php_intelephense_executable', 'intelephense')
call ale#Set('php_intelephense_use_global', 1)
call ale#Set('php_intelephense_config', {})

function! ale_linters#php#intelephense#GetProjectRoot(buffer) abort
    let l:composer_path = ale#path#FindNearestFile(a:buffer, 'composer.json')

    if (!empty(l:composer_path))
        return fnamemodify(l:composer_path, ':h')
    endif

    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

function! ale_linters#php#intelephense#GetInitializationOptions(buffer) abort
    return ale#Var(a:buffer, 'php_intelephense_config')
endfunction

call ale#linter#Define('php', {
\   'name': 'intelephense',
\   'lsp': 'stdio',
\   'initialization_options': function('ale_linters#php#intelephense#GetInitializationOptions'),
\   'executable': {b -> ale#node#FindExecutable(b, 'php_intelephense', [])},
\   'command': '%e --stdio',
\   'project_root': function('ale_linters#php#intelephense#GetProjectRoot'),
\})
