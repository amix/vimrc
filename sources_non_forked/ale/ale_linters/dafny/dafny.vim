" Author: Taylor Blau <me@ttaylorr.com>

function! ale_linters#dafny#dafny#Handle(buffer, lines) abort
    let l:pattern = '\v(.*)\((\d+),(\d+)\): (.*): (.*)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \    'bufnr': a:buffer,
        \    'col': l:match[3] + 0,
        \    'lnum': l:match[2] + 0,
        \    'text': l:match[5],
        \    'type': l:match[4] =~# '^Error' ? 'E' : 'W'
        \ })
    endfor

    return l:output
endfunction

call ale#linter#Define('dafny', {
\    'name': 'dafny',
\    'executable': 'dafny',
\    'command': 'dafny %s /compile:0',
\    'callback': 'ale_linters#dafny#dafny#Handle',
\    'lint_file': 1,
\ })
