scriptencoding utf-8
" Author: houstdav000 <houstdav000@gh0st.sh>
" Description: Fix files with nixfmt

call ale#Set('nix_nixfmt_executable', 'nixfmt')
call ale#Set('nix_nixfmt_options', '')

function! ale#fixers#nixfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'nix_nixfmt_executable')
    let l:options = ale#Var(a:buffer, 'nix_nixfmt_options')

    return {
    \   'command': ale#Escape(l:executable) . ale#Pad(l:options),
    \}
endfunction
