" Author: t2h5 <https://github.com/t2h5>
" Description: Integration of Stoplight Spectral CLI with ALE.

call ale#Set('json_spectral_executable', 'spectral')
call ale#Set('json_spectral_use_global', get(g:, 'ale_use_global_executables', 0))

call ale#linter#Define('json', {
\   'name': 'spectral',
\   'executable': {b -> ale#node#FindExecutable(b, 'json_spectral', [
\       'node_modules/.bin/spectral',
\   ])},
\   'command': '%e lint --ignore-unknown-format -q -f text %t',
\   'callback': 'ale#handlers#spectral#HandleSpectralOutput'
\})
