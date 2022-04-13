" Author: Taylor Blau <me@ttaylorr.com>

function! ale_linters#dafny#dafny#Handle(buffer, lines) abort
    let l:pattern = '\v(.*)\((\d+),(\d+)\): (.*): (.*)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \    'filename': l:match[1],
        \    'col': l:match[3] + 0,
        \    'lnum': l:match[2] + 0,
        \    'text': l:match[5],
        \    'type': l:match[4] =~# '^Error' ? 'E' : 'W'
        \ })
    endfor

    for l:match in ale#util#GetMatches(a:lines, '\v(.*)\((\d+),(\d+)\): (Verification of .{-} timed out after \d+ seconds)')
        call add(l:output, {
        \     'filename': l:match[1],
        \     'col': l:match[3] + 0,
        \     'lnum': l:match[2] + 0,
        \     'text': l:match[4],
        \     'type': 'E',
        \ })
    endfor

    return l:output
endfunction

function! ale_linters#dafny#dafny#GetCommand(buffer) abort
    return printf('dafny %%s /compile:0 /timeLimit:%d', ale#Var(a:buffer, 'dafny_dafny_timelimit'))
endfunction

call ale#Set('dafny_dafny_timelimit', 10)
call ale#linter#Define('dafny', {
\    'name': 'dafny',
\    'executable': 'dafny',
\    'command': function('ale_linters#dafny#dafny#GetCommand'),
\    'callback': 'ale_linters#dafny#dafny#Handle',
\    'lint_file': 1,
\ })
