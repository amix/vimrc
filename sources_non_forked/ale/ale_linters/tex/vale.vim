" Author: chew-z https://github.com/chew-z
" Description: vale for LaTeX files

call ale#linter#Define('tex', {
\   'name': 'vale',
\   'executable': 'vale',
\   'command': 'vale --output=JSON %t',
\   'callback': 'ale#handlers#vale#Handle',
\})
