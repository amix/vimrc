" Author: John Gentile <johncgentile17@gmail.com>
" Description: Adds support for `ghdl` VHDL compiler/checker

call ale#Set('vhdl_ghdl_executable', 'ghdl')
" Compile w/VHDL-2008 support
call ale#Set('vhdl_ghdl_options', '--std=08')

function! ale_linters#vhdl#ghdl#GetCommand(buffer) abort
    return '%e -s ' . ale#Pad(ale#Var(a:buffer, 'vhdl_ghdl_options')) . ' %t'
endfunction

function! ale_linters#vhdl#ghdl#Handle(buffer, lines) abort
    " Look for 'error' lines like the following:
    " dff_en.vhd:41:5:error: 'begin' is expected instead of 'if'
    " /path/to/file.vhdl:12:8: no declaration for "i0"
    let l:pattern = '^[a-zA-Z0-9\-\.\_\/ ]\+:\(\d\+\):\(\d\+\):\(.*\)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col' : l:match[2] + 0,
        \   'text': l:match[3],
        \   'type': 'E',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('vhdl', {
\   'name': 'ghdl',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'vhdl_ghdl_executable')},
\   'command': function('ale_linters#vhdl#ghdl#GetCommand'),
\   'callback': 'ale_linters#vhdl#ghdl#Handle',
\})
