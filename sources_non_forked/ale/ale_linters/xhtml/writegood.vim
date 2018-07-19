" Author: Sumner Evans <sumner.evans98@gmail.com>
" Description: write-good for XHTML files

call ale#linter#Define('xhtml', {
\   'name': 'write-good',
\   'executable_callback': 'ale#handlers#writegood#GetExecutable',
\   'command_callback': 'ale#handlers#writegood#GetCommand',
\   'callback': 'ale#handlers#writegood#Handle',
\})
