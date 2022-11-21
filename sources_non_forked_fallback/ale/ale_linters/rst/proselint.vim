" Author: Daniel M. Capella https://github.com/polyzen
" Description: proselint for reStructuredText files

call ale#linter#Define('rst', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
