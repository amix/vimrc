" Author: Francis Agyapong <francisagyapong2@gmail.com>
" Description: Lint kotlin files using ktlint

call ale#linter#Define('kotlin', {
\   'name': 'ktlint',
\   'executable': 'ktlint',
\   'command': function('ale#handlers#ktlint#GetCommand'),
\   'callback': 'ale#handlers#ktlint#Handle',
\   'lint_file': 1
\})
