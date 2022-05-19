scriptencoding utf-8
" Author: David Houston
" Description: This file defines a handler function for statix's errorformat
" output.

function! ale#handlers#statix#Handle(buffer, lines) abort
    " Look for lines like the following.
    "
    " flake.nix>46:13:W:3:This assignment is better written with `inherit`
    let l:pattern = '\v^.*\>(\d+):(\d+):([A-Z]):(\d+):(.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': l:match[3],
        \   'code': l:match[4],
        \   'text': l:match[5],
        \})
    endfor

    return l:output
endfunction
