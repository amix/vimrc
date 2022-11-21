" Author: rhysd <https://github.com/rhysd>
" Description: naga-cli linter for WGSL syntax.

call ale#Set('wgsl_naga_executable', 'naga')

call ale#linter#Define('wgsl', {
\   'name': 'naga',
\   'executable': {b -> ale#Var(b, 'wgsl_naga_executable')},
\   'output_stream': 'stderr',
\   'command': {b -> '%e --stdin-file-path %s'},
\   'callback': 'ale#handlers#naga#Handle',
\})
