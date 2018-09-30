" Author: richard marmorstein <https://github.com/twitchard>
" Description: plugin for Psalm, static analyzer for PHP

call ale#Set('php_psalm_executable', 'psalm')

function! ale_linters#php#psalm#Handle(buffer, lines) abort
    " Matches patterns like the following:
    let l:pattern = '^.*:\(\d\+\):\(\d\+\):\(\w\+\) - \(.*\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'text': l:match[4],
        \   'type': l:match[3][:0] is# 'e' ? 'E' : 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('php', {
\   'name': 'psalm',
\   'command': '%e --diff --output-format=emacs %s',
\   'executable_callback': ale#VarFunc('php_psalm_executable'),
\   'callback': 'ale_linters#php#psalm#Handle',
\   'lint_file': 1,
\})
