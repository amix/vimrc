" Author: chew-z https://github.com/chew-z
" Description: vale for text files

call ale#linter#Define('text', {
\   'name': 'vale',
\   'executable': 'vale',
\   'command': 'vale --output=JSON %t',
\   'callback': 'ale#handlers#vale#Handle',
\})
