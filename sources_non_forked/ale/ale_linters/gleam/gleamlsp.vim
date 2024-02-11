" Author: Jonathan Palardt https://github.com/jpalardy
" Description: Support for Gleam Language Server

call ale#Set('gleam_gleamlsp_executable', 'gleam')

function! ale_linters#gleam#gleamlsp#GetProjectRoot(buffer) abort
    let l:gleam_toml = ale#path#FindNearestFile(a:buffer, 'gleam.toml')

    return !empty(l:gleam_toml) ? fnamemodify(l:gleam_toml, ':p:h') : ''
endfunction

call ale#linter#Define('gleam', {
\   'name': 'gleamlsp',
\   'lsp': 'stdio',
\   'executable': {buffer -> ale#Var(buffer, 'gleam_gleamlsp_executable')},
\   'command': '%e lsp',
\   'project_root': function('ale_linters#gleam#gleamlsp#GetProjectRoot'),
\})
