" Author: zack <zack@kourouma.me>
" Description: Integration of hfmt with ALE.

call ale#Set('haskell_hfmt_executable', 'hfmt')

function! ale#fixers#hfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_hfmt_executable')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' -w'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction

