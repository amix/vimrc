" Author: Jeffrey Lau - https://github.com/zoonfafer
" Description: YAML Language Server https://github.com/redhat-developer/yaml-language-server

call ale#Set('yaml_ls_executable', 'yaml-language-server')
call ale#Set('yaml_ls_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('yaml_ls_config', {})

function! ale_linters#yaml#ls#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'yaml_ls', [
    \   'node_modules/.bin/yaml-language-server',
    \])
endfunction

function! ale_linters#yaml#ls#GetCommand(buffer) abort
    let l:executable = ale_linters#yaml#ls#GetExecutable(a:buffer)

    return ale#Escape(l:executable) . ' --stdio'
endfunction

" Just use the current file
function! ale_linters#yaml#ls#FindProjectRoot(buffer) abort
    let l:project_file = expand('#' . a:buffer . ':p')

    return fnamemodify(l:project_file, ':h')
endfunction

call ale#linter#Define('yaml', {
\   'name': 'yaml-language-server',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#yaml#ls#GetExecutable'),
\   'command': function('ale_linters#yaml#ls#GetCommand'),
\   'project_root': function('ale_linters#yaml#ls#FindProjectRoot'),
\   'lsp_config': {b -> ale#Var(b, 'yaml_ls_config')},
\})
