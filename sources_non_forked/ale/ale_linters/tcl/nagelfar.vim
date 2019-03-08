" Author: Nick James <github@nsjuk.xyz>
" Description: nagelfar linter for tcl files

call ale#Set('tcl_nagelfar_executable', 'nagelfar.tcl')
call ale#Set('tcl_nagelfar_options', '')

function! ale_linters#tcl#nagelfar#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'tcl_nagelfar_options')

    return '%e' . ale#Pad(l:options) . ' %s'
endfunction

function! ale_linters#tcl#nagelfar#Handle(buffer, lines) abort
    " Matches patterns like the following:
    " Line   5: W Found constant "bepa" which is also a variable.
    " Line  13: E Wrong number of arguments (3) to "set"
    " Line  93: N Close brace not aligned with line 90 (4 0)
    let l:pattern = '^Line\s\+\([0-9]\+\): \([NEW]\) \(.*\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'type': l:match[2] is# 'N' ? 'W' : l:match[2],
        \   'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('tcl', {
\   'name': 'nagelfar',
\   'output_stream': 'stdout',
\   'executable': {b -> ale#Var(b, 'tcl_nagelfar_executable')},
\   'command': function('ale_linters#tcl#nagelfar#GetCommand'),
\   'callback': 'ale_linters#tcl#nagelfar#Handle',
\   'lint_file': 1,
\})
