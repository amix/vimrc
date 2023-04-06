call ale#Set('nix_alejandra_executable', 'alejandra')
call ale#Set('nix_alejandra_options', '')

function! ale#fixers#alejandra#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'nix_alejandra_executable')
    let l:options = ale#Var(a:buffer, 'nix_alejandra_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' -- -'
    \}
endfunction
