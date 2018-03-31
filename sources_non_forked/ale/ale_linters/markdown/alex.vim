" Author: Johannes Wienke <languitar@semipol.de>
" Description: alex for markdown files

call ale#linter#Define('markdown', {
\   'name': 'alex',
\   'executable': 'alex',
\   'command': 'alex %s -t',
\   'output_stream': 'stderr',
\   'callback': 'ale#handlers#alex#Handle',
\   'lint_file': 1,
\})
