" Author: tokida https://rouger.info, Yasuhiro Kiyota <yasuhiroki.duck@gmail.com>
" Description: textlint, a proofreading tool (https://textlint.github.io/)

call ale#linter#Define('markdown', {
\   'name': 'textlint',
\   'executable_callback': 'ale#handlers#textlint#GetExecutable',
\   'command_callback': 'ale#handlers#textlint#GetCommand',
\   'callback': 'ale#handlers#textlint#HandleTextlintOutput',
\})
