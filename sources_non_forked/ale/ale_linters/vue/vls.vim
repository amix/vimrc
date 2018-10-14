" Author: Alexander Olofsson <alexander.olofsson@liu.se>
" Description: Vue vls Language Server integration for ALE

call ale#Set('vue_vls_executable', 'vls')
call ale#Set('vue_vls_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#vue#vls#GetProjectRoot(buffer) abort
    let l:package_path = ale#path#FindNearestFile(a:buffer, 'package.json')

    return !empty(l:package_path) ? fnamemodify(l:package_path, ':h') : ''
endfunction

call ale#linter#Define('vue', {
\   'name': 'vls',
\   'lsp': 'stdio',
\   'executable_callback': ale#node#FindExecutableFunc('vue_vls', [
\       'node_modules/.bin/vls',
\   ]),
\   'command': '%e --stdio',
\   'language': 'vue',
\   'project_root_callback': 'ale_linters#vue#vls#GetProjectRoot',
\})
