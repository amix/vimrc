" Author: Daniel M. Capella https://github.com/polyzen
" Description: proselint for Texinfo files

call ale#linter#Define('texinfo', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
