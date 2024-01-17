" Author: neersighted <bjorn@neersighted.com>, John Eikenberry <jae@zhar.net>
" Description: go vet for Go files

call ale#Set('go_go_executable', 'go')
call ale#Set('go_govet_options', '')

call ale#linter#Define('go', {
\   'name': 'govet',
\   'aliases': ['go vet'],
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'go_go_executable')},
\   'cwd': '%s:h',
\   'command': {b ->
\       ale#go#EnvString(b)
\       . '%e vet'
\       . ale#Pad(ale#Var(b, 'go_govet_options'))
\       . ' .'
\   },
\   'callback': 'ale#handlers#go#Handler',
\   'lint_file': 1,
\})
