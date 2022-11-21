" Author: Cian Butler https://github.com/butlerx
" Description: proselint for PO files

call ale#linter#Define('po', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
