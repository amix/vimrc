" Author: Nic West <nicwest@mailbox.org>
" Description: linter for clojure using joker https://github.com/candid82/joker

function! ale_linters#clojure#joker#HandleJokerFormat(buffer, lines) abort
    " output format
    " <filename>:<line>:<column>: <issue type>: <message>
    let l:pattern = '\v^[a-zA-Z]?:?[^:]+:(\d+):(\d+):? ((Read error|Parse error|Parse warning|Exception): ?(.+))$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:type = 'E'

        if l:match[4] is? 'Parse warning'
            let l:type = 'W'
        endif

        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[3],
        \   'type': l:type,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('clojure', {
\   'name': 'joker',
\   'output_stream': 'stderr',
\   'executable': 'joker',
\   'command': 'joker --working-dir %s --lint %t',
\   'callback': 'ale_linters#clojure#joker#HandleJokerFormat',
\})
