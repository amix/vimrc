" Author: John Nduli https://github.com/jnduli
" Description: Rstcheck for reStructuredText files
"

function! ale_linters#rst#rstcheck#Handle(buffer, lines) abort
    " matches: 'bad_rst.rst:1: (SEVERE/4) Title overline & underline
    " mismatch.'
    let l:pattern = '\v^(.+):(\d*): \(([a-zA-Z]*)/\d*\) (.+)$'
    let l:dir = expand('#' . a:buffer . ':p:h')
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'filename': ale#path#GetAbsPath(l:dir, l:match[1]),
        \   'lnum': l:match[2] + 0,
        \   'col': 0,
        \   'type': l:match[3] is# 'SEVERE' ? 'E' : 'W',
        \   'text': l:match[4],
        \})
    endfor

    return l:output
endfunction

function! ale_linters#rst#rstcheck#GetCommand(buffer) abort
    return ale#path#BufferCdString(a:buffer)
    \   . 'rstcheck'
    \   . ' %t'
endfunction


call ale#linter#Define('rst', {
\   'name': 'rstcheck',
\   'executable': 'rstcheck',
\   'command_callback': 'ale_linters#rst#rstcheck#GetCommand',
\   'callback': 'ale_linters#rst#rstcheck#Handle',
\   'output_stream': 'both',
\})
