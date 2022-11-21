" Author: jD91mZM2 <me@krake.one>
" Description: rnix-lsp language client

function! ale_linters#nix#rnix_lsp#GetProjectRoot(buffer) abort
    " rnix-lsp does not yet use the project root, so getting it right is not
    " important
    return fnamemodify(a:buffer, ':h')
endfunction

call ale#linter#Define('nix', {
\   'name': 'rnix_lsp',
\   'lsp': 'stdio',
\   'executable': 'rnix-lsp',
\   'command': '%e',
\   'project_root': function('ale_linters#nix#rnix_lsp#GetProjectRoot'),
\})
