" Author: wizzup <wizzup@gmail.com>
" Description: ghc-mod for Haskell files

call ale#Set('haskell_ghc_mod_executable', 'ghc-mod')

function! ale_linters#haskell#ghc_mod#GetCommand (buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_ghc_mod_executable')

    return ale#handlers#haskell_stack#EscapeExecutable(l:executable, 'ghc-mod')
    \   . ' --map-file %s=%t check %s'
endfunction

call ale#linter#Define('haskell', {
\   'name': 'ghc_mod',
\   'aliases': ['ghc-mod'],
\   'executable': {b -> ale#Var(b, 'haskell_ghc_mod_executable')},
\   'command': function('ale_linters#haskell#ghc_mod#GetCommand'),
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})
