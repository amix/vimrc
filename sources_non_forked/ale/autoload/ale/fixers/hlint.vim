" Author: eborden <evan@evan-borden.com>
" Description: Integration of hlint refactor with ALE.
"
call ale#Set('haskell_hlint_executable', 'hlint')

function! ale#fixers#hlint#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_hlint_executable')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' --refactor'
    \       . ' --refactor-options="--inplace"'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
