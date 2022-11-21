" Author: David Houston <houstdav000>
" Description: Provide statix fix as a fixer for simple Nix antipatterns.

call ale#Set('nix_statix_fix_executable', 'statix')
call ale#Set('nix_statix_fix_options', '')

function! ale#fixers#statix#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'nix_statix_fix_executable')
    let l:options = ale#Var(a:buffer, 'nix_statix_fix_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ale#Pad('fix')
    \       . ale#Pad('--stdin')
    \       . ale#Pad(l:options),
    \}
endfunction
