" Author: Cian Butler https://github.com/butlerx
" Description: write-good for PO files

call ale#linter#Define('po', {
\   'name': 'write-good',
\   'executable_callback': 'ale#handlers#writegood#GetExecutable',
\   'command_callback': 'ale#handlers#writegood#GetCommand',
\   'callback': 'ale#handlers#writegood#Handle',
\})
