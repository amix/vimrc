" Author: ghsang <gwonhyuksang@gmail.com>
" Description: Check Dart files with dart analyze

call ale#Set('dart_analyze_executable', 'dart')

function! ale_linters#dart#dart_analyze#Handle(buffer, lines) abort
    let l:pattern = '\v^  ([a-z]+) - (.+):(\d+):(\d+) - (.+) - (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'type': l:match[1] is# 'error' ? 'E' : 'W',
        \   'text': l:match[6] . ': ' . l:match[5],
        \   'lnum': str2nr(l:match[3]),
        \   'col': str2nr(l:match[4]),
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('dart', {
\   'name': 'dart_analyze',
\   'executable': {b -> ale#Var(b, 'dart_analyze_executable')},
\   'command': '%e analyze %s',
\   'callback': 'ale_linters#dart#dart_analyze#Handle',
\   'lint_file': 1,
\})
