" Author: Oyvind Ingvaldsen <oyvind.ingvaldsen@gmail.com>
" Description: NASM linter for asmsyntax nasm.

call ale#Set('nasm_nasm_executable', 'nasm')
call ale#Set('nasm_nasm_options', '')

function! ale_linters#nasm#nasm#GetCommand(buffer) abort
    " Note that NASM requires a trailing slash for the -I option.
    let l:separator = has('win32') ? '\' : '/'
    let l:path = fnamemodify(bufname(a:buffer), ':p:h') . l:separator
    let l:output_null = has('win32') ? 'NUL' : '/dev/null'

    return '%e -X gnu -I ' . ale#Escape(l:path)
    \   . ale#Pad(ale#Var(a:buffer, 'nasm_nasm_options'))
    \   . ' %s'
    \   . ' -o ' . l:output_null
endfunction

function! ale_linters#nasm#nasm#Handle(buffer, lines) abort
    " Note that we treat 'fatal' as errors.
    let l:pattern = '^.\+:\(\d\+\): \([^:]\+\): \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \ 'lnum': l:match[1] + 0,
        \ 'type': l:match[2] =~? 'error\|fatal' ? 'E' : 'W',
        \ 'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('nasm', {
\   'name': 'nasm',
\   'output_stream': 'stderr',
\   'lint_file': 1,
\   'executable_callback': ale#VarFunc('nasm_nasm_executable'),
\   'command_callback': 'ale_linters#nasm#nasm#GetCommand',
\   'callback': 'ale_linters#nasm#nasm#Handle',
\})
