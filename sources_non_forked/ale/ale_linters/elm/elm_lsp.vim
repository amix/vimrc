" Author: antew - https://github.com/antew
" Description: LSP integration for elm, currently supports diagnostics (linting)

call ale#Set('elm_lsp_executable', 'elm-lsp')
call ale#Set('elm_lsp_use_global', get(g:, 'ale_use_global_executables', 0))

function! elm_lsp#GetRootDir(buffer) abort
    let l:elm_json = ale#path#FindNearestFile(a:buffer, 'elm.json')

    return !empty(l:elm_json) ? fnamemodify(l:elm_json, ':p:h') : ''
endfunction

call ale#linter#Define('elm', {
\   'name': 'elm_lsp',
\   'lsp': 'stdio',
\   'executable': {b -> ale#node#FindExecutable(b, 'elm_lsp', [
\       'node_modules/.bin/elm-lsp',
\   ])},
\   'command': '%e --stdio',
\   'project_root': function('elm_lsp#GetRootDir'),
\   'language': 'elm'
\})
