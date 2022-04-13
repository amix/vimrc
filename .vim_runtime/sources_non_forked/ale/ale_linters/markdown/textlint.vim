" Author: tokida https://rouger.info, Yasuhiro Kiyota <yasuhiroki.duck@gmail.com>
" Description: textlint, a proofreading tool (https://textlint.github.io/)

call ale#linter#Define('markdown', {
\   'name': 'textlint',
\   'executable': function('ale#handlers#textlint#GetExecutable'),
\   'command': function('ale#handlers#textlint#GetCommand'),
\   'callback': 'ale#handlers#textlint#HandleTextlintOutput',
\})
