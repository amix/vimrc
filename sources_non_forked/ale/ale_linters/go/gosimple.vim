" Author: Ben Reedy <https://github.com/breed808>
" Description: gosimple for Go files

call ale#linter#Define('go', {
\   'name': 'gosimple',
\   'executable': 'gosimple',
\   'cwd': '%s:h',
\   'command': {b -> ale#go#EnvString(b) . 'gosimple .'},
\   'callback': 'ale#handlers#go#Handler',
\   'output_stream': 'both',
\   'lint_file': 1,
\})
