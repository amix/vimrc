" Author: KabbAmine - https://github.com/KabbAmine

call ale#linter#Define('sass', {
\   'name': 'sasslint',
\   'executable': 'sass-lint',
\   'command': 'sass-lint -v -q -f compact %t',
\   'callback': 'ale#handlers#css#HandleCSSLintFormat',
\})
