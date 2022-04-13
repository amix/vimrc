" Author: Jeff Kreeftmeijer https://github.com/jeffkreeftmeijer
" Description: vale for AsciiDoc files

call ale#linter#Define('asciidoc', {
\   'name': 'vale',
\   'executable': 'vale',
\   'command': 'vale --output=line %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
