" Author: TANIGUCHI Masaya <ta2gch@gmail.com>
" Description: textlint for LaTeX files

call ale#linter#Define('tex', {
\   'name': 'textlint',
\   'executable': function('ale#handlers#textlint#GetExecutable'),
\   'command': function('ale#handlers#textlint#GetCommand'),
\   'callback': 'ale#handlers#textlint#HandleTextlintOutput',
\})
