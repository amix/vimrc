" Author: ghsang <gwonhyuksang@gmail.com>
" Description: Check Dart files with dart analyze

call ale#Set('dart_analyze_executable', 'dart')

function! ale_linters#dart#dart_analyze#Handle(buffer, lines) abort
    let l:pattern = '\v([a-z]+) - (.+):(\d+):(\d+) - (.+) - (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let [l:type, l:filename, l:lnum, l:col, l:message, l:code] = l:match[1:6]
        call add(l:output, {
        \   'type': l:type is# 'error' ? 'E' : l:type is# 'info' ? 'I' : 'W',
        \   'text': l:code . ': ' . l:message,
        \   'lnum': str2nr(l:lnum),
        \   'col': str2nr(l:col),
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('dart', {
\   'name': 'dart_analyze',
\   'executable': {b -> ale#Var(b, 'dart_analyze_executable')},
\   'command': '%e analyze --fatal-infos %s',
\   'callback': 'ale_linters#dart#dart_analyze#Handle',
\   'lint_file': 1,
\})
