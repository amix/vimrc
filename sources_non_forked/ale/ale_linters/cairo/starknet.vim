" Author: 0xHyoga <0xHyoga@gmx.com>
" Description: Report starknet-compile errors in cairo code (pre-starknet
" 1.0). This is deprecated but kept for backwards compatability.

call ale#Set('cairo_starknet_executable', 'starknet-compile')
call ale#Set('cairo_starknet_options', '')

function! ale_linters#cairo#starknet#Handle(buffer, lines) abort
    " Error always on the first line
    " e.g ex01.cairo:20:6: Could not find module 'contracts.utils.ex00_base'. Searched in the following paths:
    let l:pattern = '\v\.cairo:(\d+):(\d+):+ (.*)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': str2nr(l:match[1]),
        \   'col': str2nr(l:match[2]),
        \   'type': 'E',
        \   'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

function! ale_linters#cairo#starknet#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'cairo_starknet_executable')

    return l:executable . ale#Pad(ale#Var(a:buffer, 'cairo_starknet_options')) . ' %s'
endfunction

call ale#linter#Define('cairo', {
\   'name': 'starknet',
\   'executable': {b -> ale#Var(b, 'cairo_starknet_executable')},
\   'command': function('ale_linters#cairo#starknet#GetCommand'),
\   'callback': 'ale_linters#cairo#starknet#Handle',
\   'output_stream': 'stderr',
\})

