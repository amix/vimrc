" Author: Alexander Olofsson <alexander.olofsson@liu.se>
" Description: Vue vls Language Server integration for ALE

call ale#Set('vue_vls_executable', 'vls')
call ale#Set('vue_vls_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#vue#vls#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'vue_vls', [
    \   'node_modules/.bin/vls',
    \])
endfunction

function! ale_linters#vue#vls#GetCommand(buffer) abort
    let l:exe = ale#Escape(ale_linters#vue#vls#GetExecutable(a:buffer))

    return l:exe . ' --stdio'
endfunction

function! ale_linters#vue#vls#GetProjectRoot(buffer) abort
    let l:package_path = ale#path#FindNearestFile(a:buffer, 'package.json')

    return !empty(l:package_path) ? fnamemodify(l:package_path, ':h') : ''
endfunction

call ale#linter#Define('vue', {
\   'name': 'vls',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#vue#vls#GetExecutable',
\   'command_callback': 'ale_linters#vue#vls#GetCommand',
\   'language': 'vue',
\   'project_root_callback': 'ale_linters#vue#vls#GetProjectRoot',
\})
