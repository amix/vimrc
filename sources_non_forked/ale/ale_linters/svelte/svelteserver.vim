" Author: Joakim Repomaa <joakim@repomaa.com>
" Description: Svelte Language Server integration for ALE

call ale#Set('svelte_svelteserver_executable', 'svelteserver')
call ale#Set('svelte_svelteserver_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#svelte#svelteserver#GetProjectRoot(buffer) abort
    let l:package_path = ale#path#FindNearestFile(a:buffer, 'package.json')

    return !empty(l:package_path) ? fnamemodify(l:package_path, ':h') : ''
endfunction

call ale#linter#Define('svelte', {
\   'name': 'svelteserver',
\   'lsp': 'stdio',
\   'executable': {b -> ale#node#FindExecutable(b, 'svelte_svelteserver', [
\       'node_modules/.bin/svelteserver',
\   ])},
\   'command': '%e --stdio',
\   'project_root': function('ale_linters#svelte#svelteserver#GetProjectRoot'),
\})
