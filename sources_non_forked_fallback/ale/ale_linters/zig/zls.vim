" Author: CherryMan <skipper308@hotmail.ca>
" Description: A language server for Zig

call ale#Set('zig_zls_executable', 'zls')
call ale#Set('zig_zls_config', {})

function! ale_linters#zig#zls#GetProjectRoot(buffer) abort
    let l:build_rs = ale#path#FindNearestFile(a:buffer, 'build.zig')

    return !empty(l:build_rs) ? fnamemodify(l:build_rs, ':h') : ''
endfunction

call ale#linter#Define('zig', {
\   'name': 'zls',
\   'lsp': 'stdio',
\   'lsp_config': {b -> ale#Var(b, 'zig_zls_config')},
\   'executable': {b -> ale#Var(b, 'zig_zls_executable')},
\   'command': '%e',
\   'project_root': function('ale_linters#zig#zls#GetProjectRoot'),
\})
