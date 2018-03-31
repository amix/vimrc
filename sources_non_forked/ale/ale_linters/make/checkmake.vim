" Author: aurieh - https://github.com/aurieh

function! ale_linters#make#checkmake#Handle(buffer, lines) abort
    let l:pattern = '\v^(\d+):(.+):(.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \    'bufnr': a:buffer,
        \    'lnum': l:match[1] + 0,
        \    'type': 'E',
        \    'code': l:match[2],
        \    'text': l:match[3],
        \})
    endfor
    return l:output
endfunction

call ale#linter#Define('make', {
\   'name': 'checkmake',
\   'executable': 'checkmake',
\   'command': 'checkmake %s --format="{{.LineNumber}}:{{.Rule}}:{{.Violation}}"',
\   'callback': 'ale_linters#make#checkmake#Handle',
\})
