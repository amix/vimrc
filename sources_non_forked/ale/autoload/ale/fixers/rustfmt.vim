" Author: Kelly Fox <kelly@bumfuddled.com>
" Description: Integration of rustfmt with ALE.

call ale#Set('rust_rustfmt_executable', 'rustfmt')
call ale#Set('rust_rustfmt_options', '')

function! ale#fixers#rustfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'rust_rustfmt_executable')
    let l:options = ale#Var(a:buffer, 'rust_rustfmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options),
    \}
endfunction
