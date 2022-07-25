" Author: jD91mZM2 <me@krake.one>
call ale#Set('json_jq_executable', 'jq')
call ale#Set('json_jq_options', '')
call ale#Set('json_jq_filters', '.')

" Matches patterns like the following:
" parse error: Expected another key-value pair at line 4, column 3
let s:pattern = '^parse error: \(.\+\) at line \(\d\+\), column \(\d\+\)$'

function! ale_linters#json#jq#Handle(buffer, lines) abort
    return ale#util#MapMatches(a:lines, s:pattern, {match -> {
    \   'text': match[1],
    \   'lnum': match[2] + 0,
    \   'col': match[3] + 0,
    \}})
endfunction

call ale#linter#Define('json', {
\   'name': 'jq',
\   'executable': {b -> ale#Var(b, 'json_jq_executable')},
\   'output_stream': 'stderr',
\   'command': '%e',
\   'callback': 'ale_linters#json#jq#Handle',
\})
