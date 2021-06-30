call ale#linter#Define('typescript', {
\   'name': 'xo',
\   'executable': function('ale#handlers#xo#GetExecutable'),
\   'command': function('ale#handlers#xo#GetLintCommand'),
\   'callback': 'ale#handlers#xo#HandleJSON',
\})
