" Author: Paulo Alem <paulo.alem@gmail.com>, Jake Zimmerman <jake@zimmerman.io>
" Description: Single-file SML checking with SML/NJ compiler

call ale#linter#Define('sml', {
\   'name': 'smlnj',
\   'executable_callback': 'ale#handlers#sml#GetExecutableSmlnjFile',
\   'command': 'sml',
\   'callback': 'ale#handlers#sml#Handle',
\})
