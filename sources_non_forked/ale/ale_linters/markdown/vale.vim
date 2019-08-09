" Author: chew-z https://github.com/chew-z
" Description: vale for Markdown files

call ale#linter#Define('markdown', {
\   'name': 'vale',
\   'executable': 'vale',
\   'command': 'vale --output=JSON %t',
\   'callback': 'ale#handlers#vale#Handle',
\})
