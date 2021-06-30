call ale#Set('haskell_ormolu_executable', 'ormolu')
call ale#Set('haskell_ormolu_options', '')

function! ale#fixers#ormolu#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_ormolu_executable')
    let l:options = ale#Var(a:buffer, 'haskell_ormolu_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options),
    \}
endfunction
