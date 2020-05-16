" Author: neersighted <bjorn@neersighted.com>
" Description: go vet for Go files
"
" Author: John Eikenberry <jae@zhar.net>
" Description: updated to work with go1.10
"
" Author: Ben Paxton <ben@gn32.uk>
" Description: moved to generic Golang file from govet

function! ale#handlers#go#Handler(buffer, lines) abort
    let l:pattern = '\v^([a-zA-Z]?:?[^:]+):(\d+):?(\d+)?:? ?(.+)$'
    let l:output = []
    let l:dir = expand('#' . a:buffer . ':p:h')

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'filename': ale#path#GetAbsPath(l:dir, l:match[1]),
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'text': l:match[4],
        \   'type': 'E',
        \})
    endfor

    return l:output
endfunction
