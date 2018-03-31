" Author: diegoholiveira <https://github.com/diegoholiveira>
" Description: static analyzer for PHP

" Define the minimum severity
let g:ale_php_phan_minimum_severity = get(g:, 'ale_php_phan_minimum_severity', 0)

function! ale_linters#php#phan#GetCommand(buffer) abort
    return 'phan -y '
    \   . ale#Var(a:buffer, 'php_phan_minimum_severity')
    \   . ' %s'
endfunction

function! ale_linters#php#phan#Handle(buffer, lines) abort
    " Matches against lines like the following:
    "
    " /path/to/some-filename.php:18 ERRORTYPE message
    let l:pattern = '^.*:\(\d\+\)\s\(\w\+\)\s\(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'text': l:match[3],
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('php', {
\   'name': 'phan',
\   'executable': 'phan',
\   'command_callback': 'ale_linters#php#phan#GetCommand',
\   'callback': 'ale_linters#php#phan#Handle',
\})
