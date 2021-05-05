" Author: KabbAmine <amine.kabb@gmail.com>

call ale#Set('yaml_yamllint_executable', 'yamllint')
call ale#Set('yaml_yamllint_options', '')

call ale#linter#Define('yaml', {
\   'name': 'yamllint',
\   'executable': {b -> ale#Var(b, 'yaml_yamllint_executable')},
\   'command': function('ale#handlers#yamllint#GetCommand'),
\   'callback': 'ale#handlers#yamllint#Handle',
\})
