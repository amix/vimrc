" Author: w0rp <devw0rp@gmail.com>
" Description: ghc for Haskell files, using Stack

call ale#linter#Define('haskell', {
\   'name': 'stack_ghc',
\   'aliases': ['stack-ghc'],
\   'output_stream': 'stderr',
\   'executable_callback': 'ale#handlers#haskell#GetStackExecutable',
\   'command': 'stack ghc -- -fno-code -v0 %t',
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})
