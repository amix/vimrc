" Author: Cian Butler https://github.com/butlerx
" Description: msgfmt for PO files

call ale#linter#Define('po', {
\   'name': 'msgfmt',
\   'executable': 'msgfmt',
\   'output_stream': 'stderr',
\   'command': 'msgfmt --statistics --output-file=- %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
