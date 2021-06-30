" Author: jD91mZM2 <me@krake.one>

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
endfunction

call ale#linter#Define('json', {
\   'name': 'jq',
\   'executable': function('ale#fixers#jq#GetExecutable'),
\   'output_stream': 'stderr',
\   'command': function('ale_linters#json#jq#GetCommand'),
\   'callback': 'ale_linters#json#jq#Handle',
\})
