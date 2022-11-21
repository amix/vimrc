" Author: aurieh <me@aurieh.me>
" Description: A Language Server implementation for D

call ale#Set('d_dls_executable', 'dls')

function! ale_linters#d#dls#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'd_dls_executable')
endfunction

function! ale_linters#d#dls#FindProjectRoot(buffer) abort
    " Note: this will return . if dub config is empty
    " dls can run outside DUB projects just fine
    return fnamemodify(ale#d#FindDUBConfig(a:buffer), ':h')
endfunction

call ale#linter#Define('d', {
\   'name': 'dls',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#d#dls#GetExecutable'),
\   'command': function('ale_linters#d#dls#GetExecutable'),
\   'project_root': function('ale_linters#d#dls#FindProjectRoot'),
\})
