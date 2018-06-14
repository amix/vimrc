" Author: Oyvind Ingvaldsen <oyvind.ingvaldsen@gmail.com>
" Description: NASM linter for asmsyntax nasm.

call ale#Set('nasm_nasm_executable', 'nasm')
call ale#Set('nasm_nasm_options', '')

function! ale_linters#nasm#nasm#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'nasm_nasm_executable')
endfunction

function! ale_linters#nasm#nasm#GetOptions(buffer) abort
    return ale#Var(a:buffer, 'nasm_nasm_options')
endfunction

function! ale_linters#nasm#nasm#GetCommand(buffer) abort
    " Note that NASM require a trailing slash to the -I option.
    let l:executable = ale#Escape(ale_linters#nasm#nasm#GetExecutable(a:buffer))
    let l:separator = has('win32') ? '\' : '/'
    let l:path = ale#Escape(fnamemodify(bufname(a:buffer), ':p:h') . l:separator)
    let l:options = ale_linters#nasm#nasm#GetOptions(a:buffer)

    return l:executable
    \   . ' -X gnu'
    \   . ' -I ' . l:path
    \   . ' ' . l:options
    \   . ' %s'
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
\   'executable': 'nasm',
\   'output_stream': 'stderr',
\   'lint_file': 1,
\   'command_callback': 'ale_linters#nasm#nasm#GetCommand',
\   'callback': 'ale_linters#nasm#nasm#Handle',
\})
