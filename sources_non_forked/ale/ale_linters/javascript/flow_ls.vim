" Author: t_t <jamestthompson3@gmail.com>
" Description: Integrate ALE with flow-language-server.

call ale#Set('javascript_flow_ls_executable', 'flow')
call ale#Set('javascript_flow_ls_use_global',
\    get(g:, 'ale_use_global_executables', 0)
\)

function! ale_linters#javascript#flow_ls#FindProjectRoot(buffer) abort
    let l:flow_config = ale#path#FindNearestFile(a:buffer, '.flowconfig')

    if !empty(l:flow_config)
        return fnamemodify(l:flow_config, ':h')
    endif

    return ''
endfunction

call ale#linter#Define('javascript', {
\   'name': 'flow-language-server',
\   'lsp': 'stdio',
\   'executable_callback': ale#node#FindExecutableFunc('javascript_flow_ls', [
\       'node_modules/.bin/flow',
\   ]),
\   'command': '%e lsp --from ale-lsp',
\   'project_root_callback': 'ale_linters#javascript#flow_ls#FindProjectRoot',
\   'language': 'javascript',
\})
