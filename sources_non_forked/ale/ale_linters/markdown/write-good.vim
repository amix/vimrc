" Author: Sumner Evans <sumner.evans98@gmail.com>
" Description: write-good for Markdown files

call ale#linter#Define('markdown', {
\   'name': 'write-good',
\   'executable_callback': 'ale#handlers#writegood#GetExecutable',
\   'command_callback': 'ale#handlers#writegood#GetCommand',
\   'callback': 'ale#handlers#writegood#Handle',
\})
