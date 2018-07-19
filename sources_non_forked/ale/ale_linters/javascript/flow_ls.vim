" Author: t_t <jamestthompson3@gmail.com>
" Description: Integrate ALE with flow-language-server.

call ale#Set('javascript_flow_ls_executable', 'flow')
call ale#Set('javascript_flow_ls_use_global',
\    get(g:, 'ale_use_global_executables', 0)
\)

function! ale_linters#javascript#flow_ls#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_flow_ls', [
    \   'node_modules/.bin/flow',
    \])
endfunction

function! ale_linters#javascript#flow_ls#GetCommand(buffer) abort
    let l:executable = ale_linters#javascript#flow_ls#GetExecutable(a:buffer)

    return ale#Escape(l:executable) . ' lsp --from ale-lsp'
endfunction

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
\   'executable_callback': 'ale_linters#javascript#flow_ls#GetExecutable',
\   'command_callback': 'ale_linters#javascript#flow_ls#GetCommand',
\   'project_root_callback': 'ale_linters#javascript#flow_ls#FindProjectRoot',
\   'language': 'javascript',
\})
