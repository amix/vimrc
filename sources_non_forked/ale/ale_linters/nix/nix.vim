" Author: Alistair Bill <@alibabzo>
" Description: nix-instantiate linter for nix files

function! ale_linters#nix#nix#Handle(buffer, lines) abort
    let l:pattern = '^\(.\+\): \(.\+\), at .*:\(\d\+\):\(\d\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[3] + 0,
        \   'col': l:match[4] + 0,
        \   'text': l:match[1] . ': ' . l:match[2],
        \   'type': l:match[1] =~# '^error' ? 'E' : 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('nix', {
\   'name': 'nix',
\   'output_stream': 'stderr',
\   'executable': 'nix-instantiate',
\   'command': 'nix-instantiate --parse -',
\   'callback': 'ale_linters#nix#nix#Handle',
\})
