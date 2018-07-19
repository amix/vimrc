" Author: Sumner Evans <sumner.evans98@gmail.com>
" Description: write-good for vim Help files

call ale#linter#Define('help', {
\   'name': 'write-good',
\   'executable_callback': 'ale#handlers#writegood#GetExecutable',
\   'command_callback': 'ale#handlers#writegood#GetCommand',
\   'callback': 'ale#handlers#writegood#Handle',
\})
