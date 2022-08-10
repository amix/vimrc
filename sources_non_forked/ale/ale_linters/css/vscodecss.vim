" Author: Dalius Dobravolskas <dalius.dobravolskas@gmail.com>
" Description: VSCode css language server

function! ale_linters#css#vscodecss#GetProjectRoot(buffer) abort
    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

call ale#linter#Define('css', {
\   'name': 'vscodecss',
\   'lsp': 'stdio',
\   'executable': 'vscode-css-language-server',
\   'command': '%e --stdio',
\   'project_root': function('ale_linters#css#vscodecss#GetProjectRoot'),
\})
