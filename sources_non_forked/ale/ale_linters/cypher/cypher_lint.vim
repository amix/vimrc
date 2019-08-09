" Author: Francisco Lopes <francisco@oblita.com>
" Description: Linting for Neo4j's Cypher

function! ale_linters#cypher#cypher_lint#Handle(buffer, lines) abort
    let l:pattern = '\v^([a-zA-Z]?:?[^:]+):(\d+):(\d+): (.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'text': l:match[4],
        \   'type': 'E',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('cypher', {
\   'name': 'cypher_lint',
\   'executable': 'cypher-lint',
\   'command': 'cypher-lint',
\   'output_stream': 'stderr',
\   'callback': 'ale_linters#cypher#cypher_lint#Handle',
\})
