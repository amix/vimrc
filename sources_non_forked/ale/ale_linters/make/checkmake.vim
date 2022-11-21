" Author: aurieh - https://github.com/aurieh

call ale#Set('make_checkmake_config', '')

function! ale_linters#make#checkmake#Handle(buffer, lines) abort
    let l:pattern = '\v^(\d+):(.+):(.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \    'bufnr': a:buffer,
        \    'lnum': l:match[1] + 0,
        \    'type': 'E',
        \    'code': l:match[2],
        \    'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

function! ale_linters#make#checkmake#GetCommand(buffer) abort
    let l:config = ale#Var(a:buffer, 'make_checkmake_config')
    let l:cmd = 'checkmake'
    \   . ' --format="{{.LineNumber}}:{{.Rule}}:{{.Violation}}{{\"\r\n\"}}"'
    \   . (!empty(l:config) ? ' --config="' . l:config . '"' : '')
    \   . ' %s'

    return l:cmd
endfunction

call ale#linter#Define('make', {
\   'name': 'checkmake',
\   'executable': 'checkmake',
\   'command': function('ale_linters#make#checkmake#GetCommand'),
\   'callback': 'ale_linters#make#checkmake#Handle',
\})
