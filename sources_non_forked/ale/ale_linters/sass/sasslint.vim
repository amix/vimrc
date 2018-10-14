" Author: KabbAmine - https://github.com/KabbAmine,
"   Ben Falconer <ben@falconers.me.uk>

call ale#linter#Define('sass', {
\   'name': 'sasslint',
\   'executable': 'sass-lint',
\   'command_callback': 'ale#handlers#sasslint#GetCommand',
\   'callback': 'ale#handlers#css#HandleCSSLintFormat',
\})
