" Author: Daniel M. Capella https://github.com/polyzen
" Description: proselint for Pod files

call ale#linter#Define('pod', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
