" Author: chew-z https://github.com/chew-z
" Description: vale for RST files

call ale#linter#Define('rst', {
\   'name': 'vale',
\   'executable': 'vale',
\   'command': 'vale --output=JSON %t',
\   'callback': 'ale#handlers#vale#Handle',
\})
