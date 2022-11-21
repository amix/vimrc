call ale#Set('yaml_yamllint_executable', 'yamllint')
call ale#Set('yaml_yamllint_options', '')

call ale#linter#Define('openapi', {
\   'name': 'yamllint',
\   'executable': {b -> ale#Var(b, 'yaml_yamllint_executable')},
\   'command': function('ale#handlers#yamllint#GetCommand'),
\   'callback': 'ale#handlers#yamllint#Handle',
\})
