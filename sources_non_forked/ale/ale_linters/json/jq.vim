" Author: jD91mZM2 <me@krake.one>
<<<<<<< HEAD

function! ale_linters#json#jq#GetCommand(buffer) abort
    let l:executable = ale#fixers#jq#GetExecutable(a:buffer)

    return ale#Escape(l:executable)
endfunction

function! ale_linters#json#jq#Handle(buffer, lines) abort
    " Matches patterns like the following:
    " parse error: Expected another key-value pair at line 4, column 3
    let l:pattern = '^parse error: \(.\+\) at line \(\d\+\), column \(\d\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'text': l:match[1],
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \})
    endfor

    return l:output
=======
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
>>>>>>> 1cca3b1df2973096bb9526a0d79c7b93c04e66b3
endfunction

call ale#linter#Define('json', {
\   'name': 'jq',
<<<<<<< HEAD
\   'executable': function('ale#fixers#jq#GetExecutable'),
\   'output_stream': 'stderr',
\   'command': function('ale_linters#json#jq#GetCommand'),
=======
\   'executable': {b -> ale#Var(b, 'json_jq_executable')},
\   'output_stream': 'stderr',
\   'command': '%e',
>>>>>>> 1cca3b1df2973096bb9526a0d79c7b93c04e66b3
\   'callback': 'ale_linters#json#jq#Handle',
\})
