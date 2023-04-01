" Author: uidops <uidops@protonmail.com>
" Description: llvm-mc linter for asm files

call ale#Set('asm_llvm_mc_executable', 'llvm-mc')
call ale#Set('asm_llvm_mc_options', '')

function! ale_linters#asm#llvm_mc#GetCommand(buffer) abort
    return '%e --assemble'
    \  . ' --filetype=asm'
    \  . ' -o ' . g:ale#util#nul_file
    \  . ' ' . ale#Var(a:buffer, 'asm_llvm_mc_options')
endfunction

function! ale_linters#asm#llvm_mc#Handle(buffer, lines) abort
    let l:pattern = '^.\+:\(\d\+\):\(\d\+\): \([^:]\+\): \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \  'lnum': l:match[1] + 0,
        \  'col': l:match[2] + 0,
        \  'type': l:match[3] =~? 'error' ? 'E' : 'W',
        \  'text': l:match[4],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('asm', {
\    'name': 'llvm_mc',
\    'output_stream': 'stderr',
\    'executable': {b -> ale#Var(b, 'asm_llvm_mc_executable')},
\    'command': function('ale_linters#asm#llvm_mc#GetCommand'),
\    'callback': 'ale_linters#asm#llvm_mc#Handle',
\})

