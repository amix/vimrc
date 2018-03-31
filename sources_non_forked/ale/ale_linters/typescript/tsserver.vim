" Author: w0rp <devw0rp@gmail.com>
" Description: tsserver integration for ALE

call ale#Set('typescript_tsserver_executable', 'tsserver')
call ale#Set('typescript_tsserver_config_path', '')
call ale#Set('typescript_tsserver_use_global', 0)

" These functions need to be defined just to comply with the API for LSP.
function! ale_linters#typescript#tsserver#GetProjectRoot(buffer) abort
    return ''
endfunction

function! ale_linters#typescript#tsserver#GetLanguage(buffer) abort
    return ''
endfunction

function! ale_linters#typescript#tsserver#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'typescript_tsserver', [
    \   'node_modules/.bin/tsserver',
    \])
endfunction

call ale#linter#Define('typescript', {
\   'name': 'tsserver',
\   'lsp': 'tsserver',
\   'executable_callback': 'ale_linters#typescript#tsserver#GetExecutable',
\   'command_callback': 'ale_linters#typescript#tsserver#GetExecutable',
\   'project_root_callback': 'ale_linters#typescript#tsserver#GetProjectRoot',
\   'language_callback': 'ale_linters#typescript#tsserver#GetLanguage',
\})
