" Author: AntoineGagne - https://github.com/AntoineGagne
" Description: Integration of erlfmt with ALE.

call ale#Set('erlang_erlfmt_executable', 'erlfmt')
call ale#Set('erlang_erlfmt_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('erlang_erlfmt_options', '')

function! ale#fixers#erlfmt#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'erlang_erlfmt', ['erlfmt'])
endfunction

function! ale#fixers#erlfmt#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'erlang_erlfmt_options')
    let l:executable = ale#fixers#erlfmt#GetExecutable(a:buffer)

    let l:command = ale#Escape(l:executable) . (empty(l:options) ? '' : ' ' . l:options) . ' %s'

    return {
    \   'command': l:command
    \}
endfunction
