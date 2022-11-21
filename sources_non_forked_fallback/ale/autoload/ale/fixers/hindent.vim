" Author: AlexeiDrake <drake.alexei@gmail.com>
" Description: Integration of hindent formatting with ALE.
"
call ale#Set('haskell_hindent_executable', 'hindent')

function! ale#fixers#hindent#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_hindent_executable')

    return ale#handlers#haskell_stack#EscapeExecutable(l:executable, 'hindent')
endfunction

function! ale#fixers#hindent#Fix(buffer) abort
    let l:executable = ale#fixers#hindent#GetExecutable(a:buffer)

    return {
    \   'command': l:executable
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
