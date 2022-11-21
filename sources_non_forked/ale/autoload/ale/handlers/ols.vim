" Author: Michael Jungo <michaeljungo92@gmail.com>
" Description: Handlers for the OCaml language server

function! ale#handlers#ols#GetExecutable(buffer) abort
    let l:ols_setting = ale#handlers#ols#GetLanguage(a:buffer) . '_ols'

    return ale#path#FindExecutable(a:buffer, l:ols_setting, [
    \   'node_modules/.bin/ocaml-language-server',
    \])
endfunction

function! ale#handlers#ols#GetCommand(buffer) abort
    let l:executable = ale#handlers#ols#GetExecutable(a:buffer)

    return ale#node#Executable(a:buffer, l:executable) . ' --stdio'
endfunction

function! ale#handlers#ols#GetLanguage(buffer) abort
    return getbufvar(a:buffer, '&filetype')
endfunction

function! ale#handlers#ols#GetProjectRoot(buffer) abort
    let l:merlin_file = ale#path#FindNearestFile(a:buffer, '.merlin')

    return !empty(l:merlin_file) ? fnamemodify(l:merlin_file, ':h') : ''
endfunction
