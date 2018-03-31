" Author: eborden <evan@evan-borden.com>
" Description: Integration of brittany with ALE.

call ale#Set('haskell_brittany_executable', 'brittany')

function! ale#fixers#brittany#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_brittany_executable')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction

