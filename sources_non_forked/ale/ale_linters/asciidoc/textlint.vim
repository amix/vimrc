" Author: TANIGUCHI Masaya <ta2gch@gmail.com>
" Description: textlint for AsciiDoc files

call ale#linter#Define('asciidoc', {
\   'name': 'textlint',
\   'executable': function('ale#handlers#textlint#GetExecutable'),
\   'command': function('ale#handlers#textlint#GetCommand'),
\   'callback': 'ale#handlers#textlint#HandleTextlintOutput',
\})
