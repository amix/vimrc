" Author: Lucas Kolstad <lkolstad@uw.edu>
" Description: gcc linter for asm files

call ale#Set('asm_gcc_executable', 'gcc')
call ale#Set('asm_gcc_options', '-Wall')

function! ale_linters#asm#gcc#GetCommand(buffer) abort
    " `-o /dev/null` or `-o null` is needed to catch all errors,
    " -fsyntax-only doesn't catch everything.
    return '%e -x assembler'
    \   . ' -o ' . g:ale#util#nul_file
    \   . '-iquote %s:h'
    \   . ' ' . ale#Var(a:buffer, 'asm_gcc_options') . ' -'
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
\    'executable': {b -> ale#Var(b, 'asm_gcc_executable')},
\    'command': function('ale_linters#asm#gcc#GetCommand'),
\    'callback': 'ale_linters#asm#gcc#Handle',
\})
