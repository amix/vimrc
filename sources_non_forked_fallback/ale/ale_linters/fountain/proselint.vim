" Author: Jansen Mitchell https://github.com/JansenMitchell
" Description: proselint for Fountain files

call ale#linter#Define('fountain', {
\    'name': 'proselint',
\    'executable': 'proselint',
\    'command': 'proselint %t',
\    'callback': 'ale#handlers#unix#HandleAsWarning',
\})
