" Author: Eric Wolf <ericwolf42@gmail.com>
" Description: ghc for Haskell files called with cabal exec

call ale#Set('haskell_cabal_ghc_options', '-fno-code -v0')

function! ale_linters#haskell#cabal_ghc#GetCommand(buffer) abort
    return 'cabal exec -- ghc '
    \   . ale#Var(a:buffer, 'haskell_cabal_ghc_options')
    \   . ' %t'
endfunction

call ale#linter#Define('haskell', {
\   'name': 'cabal_ghc',
\   'aliases': ['cabal-ghc'],
\   'output_stream': 'stderr',
\   'executable': 'cabal',
\   'command': function('ale_linters#haskell#cabal_ghc#GetCommand'),
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})
