" Author: iclanzan <sorin@iclanzan.com>
" Description: Integration of purty with ALE.

call ale#Set('purescript_purty_executable', 'purty')

function! ale#fixers#purty#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'purescript_purty_executable')

    return ale#Escape(l:executable)
endfunction

function! ale#fixers#purty#Fix(buffer) abort
    let l:executable = ale#fixers#purty#GetExecutable(a:buffer)

    return {
    \   'command': l:executable
    \       . ' --write'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction

