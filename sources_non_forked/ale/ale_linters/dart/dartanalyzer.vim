" Author: w0rp <devw0rp@gmail.com>
" Description: Check Dart files with dartanalyzer

call ale#Set('dart_dartanalyzer_executable', 'dartanalyzer')

function! ale_linters#dart#dartanalyzer#GetCommand(buffer) abort
    let l:path = ale#path#FindNearestFile(a:buffer, '.packages')

    return '%e'
    \   . (!empty(l:path) ? ' --packages ' . ale#Escape(l:path) : '')
    \   . ' %s'
endfunction

function! ale_linters#dart#dartanalyzer#Handle(buffer, lines) abort
    let l:pattern = '\v^  ([a-z]+) . (.+) at (.+):(\d+):(\d+) . (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'type': l:match[1] is# 'error' ? 'E' : 'W',
        \   'text': l:match[6] . ': ' . l:match[2],
        \   'lnum': str2nr(l:match[4]),
        \   'col': str2nr(l:match[5]),
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('dart', {
\   'name': 'dartanalyzer',
\   'executable': {b -> ale#Var(b, 'dart_dartanalyzer_executable')},
\   'command': function('ale_linters#dart#dartanalyzer#GetCommand'),
\   'callback': 'ale_linters#dart#dartanalyzer#Handle',
\   'lint_file': 1,
\})
