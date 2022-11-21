call ale#Set('nix_nixpkgsfmt_executable', 'nixpkgs-fmt')
call ale#Set('nix_nixpkgsfmt_options', '')

function! ale#fixers#nixpkgsfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'nix_nixpkgsfmt_executable')
    let l:options = ale#Var(a:buffer, 'nix_nixpkgsfmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options),
    \}
endfunction
