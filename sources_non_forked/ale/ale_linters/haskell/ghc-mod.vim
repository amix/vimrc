" Author: wizzup <wizzup@gmail.com>
" Description: ghc-mod for Haskell files

call ale#linter#Define('haskell', {
\   'name': 'ghc-mod',
\   'executable': 'ghc-mod',
\   'command': 'ghc-mod --map-file %s=%t check %s',
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})

call ale#linter#Define('haskell', {
\   'name': 'stack-ghc-mod',
\   'executable': 'stack',
\   'command': 'stack exec ghc-mod -- --map-file %s=%t check %s',
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})
