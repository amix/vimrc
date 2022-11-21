" Author: bretello <bretello@distruzione.org>

call ale#Set('yaml_actionlint_executable', 'actionlint')
call ale#Set('yaml_actionlint_options', '')

call ale#linter#Define('yaml', {
\   'name': 'actionlint',
\   'executable': {b -> ale#Var(b, 'yaml_actionlint_executable')},
\   'command': function('ale#handlers#actionlint#GetCommand'),
\   'callback': 'ale#handlers#actionlint#Handle',
\})
