" Author: robertjlooby <robertjlooby@gmail.com>
" Description: Integration of floskell with ALE.

call ale#Set('haskell_floskell_executable', 'floskell')

function! ale#fixers#floskell#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_floskell_executable')

    return ale#handlers#haskell_stack#EscapeExecutable(l:executable, 'floskell')
endfunction

function! ale#fixers#floskell#Fix(buffer) abort
    let l:executable = ale#fixers#floskell#GetExecutable(a:buffer)

    return {
    \   'command': l:executable
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
