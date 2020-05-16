" Author: Fohte (Hayato Kawai) https://github.com/fohte
" Description: Integration of Rufo with ALE.

call ale#Set('ruby_rufo_executable', 'rufo')

function! ale#fixers#rufo#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ruby_rufo_executable')
    let l:exec_args = l:executable =~? 'bundle$'
    \   ? ' exec rufo'
    \   : ''

    return ale#Escape(l:executable) . l:exec_args . ' %t'
endfunction

function! ale#fixers#rufo#Fix(buffer) abort
    return {
    \   'command': ale#fixers#rufo#GetCommand(a:buffer),
    \   'read_temporary_file': 1,
    \}
endfunction
