" Author: Daniel M. Capella https://github.com/polyzen
" Description: proselint for nroff files

call ale#linter#Define('nroff', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
