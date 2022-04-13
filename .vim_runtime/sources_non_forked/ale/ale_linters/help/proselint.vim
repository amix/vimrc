" Author: Daniel M. Capella https://github.com/polyzen
" Description: proselint for Vim help files

call ale#linter#Define('help', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
