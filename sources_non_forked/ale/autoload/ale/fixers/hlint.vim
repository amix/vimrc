" Author: eborden <evan@evan-borden.com>
" Description: Integration of hlint refactor with ALE.
"

function! ale#fixers#hlint#Fix(buffer) abort
    return {
    \   'command': ale#handlers#hlint#GetExecutable(a:buffer)
    \       . ' --refactor'
    \       . ' --refactor-options="--inplace"'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
