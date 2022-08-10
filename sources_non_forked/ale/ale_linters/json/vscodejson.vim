" Author: Dalius Dobravolskas <dalius.dobravolskas@gmail.com>
" Description: VSCode json language server

function! ale_linters#json#vscodejson#GetProjectRoot(buffer) abort
    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

call ale#linter#Define('json', {
\   'name': 'vscodejson',
\   'lsp': 'stdio',
\   'executable': 'vscode-json-language-server',
\   'command': '%e --stdio',
\   'project_root': function('ale_linters#json#vscodejson#GetProjectRoot'),
\})
