" Author: w0rp <devw0rp@gmail.com>
" Description: ghc for Haskell files, using Stack

call ale#Set('haskell_stack_ghc_options', '-fno-code -v0')

function! ale_linters#haskell#stack_ghc#GetCommand(buffer) abort
    return ale#handlers#haskell#GetStackExecutable(a:buffer)
    \ . ' ghc -- '
    \ . ale#Var(a:buffer, 'haskell_stack_ghc_options')
    \ . ' %t'
endfunction

call ale#linter#Define('haskell', {
\   'name': 'stack_ghc',
\   'aliases': ['stack-ghc'],
\   'output_stream': 'stderr',
\   'executable': function('ale#handlers#haskell#GetStackExecutable'),
\   'command': function('ale_linters#haskell#stack_ghc#GetCommand'),
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})
