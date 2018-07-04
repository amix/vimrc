" Author: Chaucerbao, w0rp <devw0rp@gmail.com>
" Description: tsserver integration for ALE

call ale#Set('javascript_tsserver_executable', 'tsserver')
call ale#Set('javascript_tsserver_config_path', '')
call ale#Set('javascript_tsserver_use_global', get(g:, 'ale_use_global_executables', 0))

" These functions need to be defined just to comply with the API for LSP.
function! ale_linters#javascript#tsserver#GetProjectRoot(buffer) abort
    return ''
endfunction

function! ale_linters#javascript#tsserver#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_tsserver', [
    \   'node_modules/.bin/tsserver',
    \])
endfunction

call ale#linter#Define('javascript', {
\   'name': 'tsserver',
\   'lsp': 'tsserver',
\   'executable_callback': 'ale_linters#javascript#tsserver#GetExecutable',
\   'command_callback': 'ale_linters#javascript#tsserver#GetExecutable',
\   'project_root_callback': 'ale_linters#javascript#tsserver#GetProjectRoot',
\   'language': '',
\})
