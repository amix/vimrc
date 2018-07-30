" Author: wizzup <wizzup@gmail.com>
" Description: ghc-mod for Haskell files

call ale#linter#Define('haskell', {
\   'name': 'ghc_mod',
\   'aliases': ['ghc-mod'],
\   'executable': 'ghc-mod',
\   'command': 'ghc-mod --map-file %s=%t check %s',
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})

call ale#linter#Define('haskell', {
\   'name': 'stack_ghc_mod',
\   'aliases': ['stack-ghc-mod'],
\   'executable': 'stack',
\   'command': 'stack exec ghc-mod -- --map-file %s=%t check %s',
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})
