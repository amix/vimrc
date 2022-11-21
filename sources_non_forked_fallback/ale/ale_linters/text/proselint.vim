" Author: poohzrn https://github.com/poohzrn
" Description: proselint for text files

call ale#linter#Define('text', {
\   'name': 'proselint',
\   'executable': 'proselint',
\   'command': 'proselint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
