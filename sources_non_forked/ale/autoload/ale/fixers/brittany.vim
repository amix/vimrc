" Author: eborden <evan@evan-borden.com>, ifyouseewendy <ifyouseewendy@gmail.com>, aspidiets <emarshall85@gmail.com>
" Description: Integration of brittany with ALE.

call ale#Set('haskell_brittany_executable', 'brittany')

function! ale#fixers#brittany#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_brittany_executable')

    return ale#handlers#haskell_stack#EscapeExecutable(l:executable, 'brittany')
endfunction

function! ale#fixers#brittany#Fix(buffer) abort
    let l:executable = ale#fixers#brittany#GetExecutable(a:buffer)

    return {
    \   'command': l:executable
    \       . ' --write-mode inplace'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction

