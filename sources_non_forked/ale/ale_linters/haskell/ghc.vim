" Author: w0rp <devw0rp@gmail.com>
" Description: ghc for Haskell files

call ale#Set('haskell_ghc_options', '-fno-code -v0')

function! ale_linters#haskell#ghc#GetCommand(buffer) abort
    return 'ghc '
    \   . ale#Var(a:buffer, 'haskell_ghc_options')
    \   . ' %t'
endfunction

call ale#linter#Define('haskell', {
\   'name': 'ghc',
\   'output_stream': 'stderr',
\   'executable': 'ghc',
\   'command': function('ale_linters#haskell#ghc#GetCommand'),
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})
