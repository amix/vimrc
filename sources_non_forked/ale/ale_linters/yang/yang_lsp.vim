call ale#Set('yang_lsp_executable', 'yang-language-server')

function! ale_linters#yang#yang_lsp#GetProjectRoot(buffer) abort
    let l:project_root = ale#path#FindNearestFile(a:buffer, 'yang.settings')

    return !empty(l:project_root) ? fnamemodify(l:project_root, ':h') : ''
endfunction

call ale#linter#Define('yang', {
\   'name': 'yang_lsp',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'yang_lsp_executable')},
\   'project_root': function('ale_linters#yang#yang_lsp#GetProjectRoot'),
\   'command': '%e',
\})
