" Author: Benjamin Block <https://github.com/benjamindblock>
" Description: A language server for Odin.

function! ale_linters#odin#ols#GetProjectRoot(buffer) abort
    return fnamemodify('', ':h')
endfunction

call ale#Set('odin_ols_executable', 'ols')
call ale#Set('odin_ols_config', {})

call ale#linter#Define('odin', {
\   'name': 'ols',
\   'lsp': 'stdio',
\   'language': 'odin',
\   'lsp_config': {b -> ale#Var(b, 'odin_ols_config')},
\   'executable': {b -> ale#Var(b, 'odin_ols_executable')},
\   'command': '%e',
\   'project_root': function('ale_linters#odin#ols#GetProjectRoot'),
\})
