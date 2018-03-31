" Author: Lucas Kolstad <lkolstad@uw.edu>
" Description: gcc linter for asm files

call ale#Set('asm_gcc_executable', 'gcc')
call ale#Set('asm_gcc_options', '-Wall')

function! ale_linters#asm#gcc#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'asm_gcc_executable')
endfunction

function! ale_linters#asm#gcc#GetCommand(buffer) abort
    return ale#Escape(ale_linters#asm#gcc#GetExecutable(a:buffer))
    \    . ' -x assembler -fsyntax-only '
    \    . '-iquote ' . ale#Escape(fnamemodify(bufname(a:buffer), ':p:h'))
    \    . ' ' . ale#Var(a:buffer, 'asm_gcc_options') . ' -'
endfunction

function! ale_linters#asm#gcc#Handle(buffer, lines) abort
    let l:pattern = '^.\+:\(\d\+\): \([^:]\+\): \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \ 'lnum': l:match[1] + 0,
        \ 'type': l:match[2] =~? 'error' ? 'E' : 'W',
        \ 'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('asm', {
\    'name': 'gcc',
\    'output_stream': 'stderr',
\    'executable_callback': 'ale_linters#asm#gcc#GetExecutable',
\    'command_callback': 'ale_linters#asm#gcc#GetCommand',
\    'callback': 'ale_linters#asm#gcc#Handle',
\})
