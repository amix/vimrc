" Author: TANIGUCHI Masaya <ta2gch@gmail.com>
" Description: Integration of textlint with ALE.

function! ale#fixers#textlint#Fix(buffer) abort
    let l:executable = ale#handlers#textlint#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'textlint_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' --fix'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
