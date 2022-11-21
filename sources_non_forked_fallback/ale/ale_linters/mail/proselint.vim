" Author: Daniel M. Capella https://github.com/polyzen
" Description: proselint for mail files

call ale#linter#Define('mail', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
