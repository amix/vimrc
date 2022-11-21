" Author: eborden <evan@evan-borden.com>
" Description: Integration of stylish-haskell formatting with ALE.
"
call ale#Set('haskell_stylish_haskell_executable', 'stylish-haskell')

function! ale#fixers#stylish_haskell#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_stylish_haskell_executable')

    return ale#handlers#haskell_stack#EscapeExecutable(l:executable, 'stylish-haskell')
endfunction

function! ale#fixers#stylish_haskell#Fix(buffer) abort
    let l:executable = ale#fixers#stylish_haskell#GetExecutable(a:buffer)

    return {
    \   'command': l:executable
    \       . ' --inplace'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
