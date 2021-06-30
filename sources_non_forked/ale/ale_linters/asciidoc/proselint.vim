" Author: Daniel M. Capella https://github.com/polyzen
" Description: proselint for AsciiDoc files

call ale#linter#Define('asciidoc', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
