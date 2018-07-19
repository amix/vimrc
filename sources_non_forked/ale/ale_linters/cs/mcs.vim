let g:ale_cs_mcs_options = get(g:, 'ale_cs_mcs_options', '')

function! ale_linters#cs#mcs#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'cs_mcs_options')

    return 'mcs -unsafe --parse'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' %t'
endfunction

function! ale_linters#cs#mcs#Handle(buffer, lines) abort
    " Look for lines like the following.
    "
    " Tests.cs(12,29): error CSXXXX: ; expected
    let l:pattern = '^\v(.+\.cs)\((\d+),(\d+)\)\: ([^ ]+) ([^ ]+): (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'type': l:match[4] is# 'error' ? 'E' : 'W',
        \   'code': l:match[5],
        \   'text': l:match[6],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('cs',{
\   'name': 'mcs',
\   'output_stream': 'stderr',
\   'executable': 'mcs',
\   'command_callback': 'ale_linters#cs#mcs#GetCommand',
\   'callback': 'ale_linters#cs#mcs#Handle',
\})
