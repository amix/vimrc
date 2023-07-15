call ale#Set('haskell_fourmolu_executable', 'fourmolu')
call ale#Set('haskell_fourmolu_options', '')

function! ale#fixers#fourmolu#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_fourmolu_executable')

    return ale#handlers#haskell_stack#EscapeExecutable(l:executable, 'fourmolu')
endfunction

function! ale#fixers#fourmolu#Fix(buffer) abort
    let l:executable = ale#fixers#fourmolu#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'haskell_fourmolu_options')

    return {
    \   'command': l:executable
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' --stdin-input-file '
    \       . ale#Escape(@%),
    \}
endfunction
