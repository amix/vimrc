" Author: Daniel M. Capella https://github.com/polyzen
" Description: proselint for HTML files

call ale#linter#Define('html', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
