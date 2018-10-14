" Author: Daniel M. Capella https://github.com/polyzen
" Description: proselint for XHTML files

call ale#linter#Define('xhtml', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
