" Author: eborden <evan@evan-borden.com>
" Description: Integration of stylish-haskell formatting with ALE.
"
call ale#Set('haskell_stylish_haskell_executable', 'stylish-haskell')

function! ale#fixers#stylish_haskell#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_stylish_haskell_executable')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' --inplace'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
