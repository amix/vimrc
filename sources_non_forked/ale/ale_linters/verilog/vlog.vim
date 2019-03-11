" Author:      John Gentile <johncgentile17@gmail.com>
" Description: Adds support for Mentor Graphics Questa/ModelSim `vlog` Verilog compiler/checker

call ale#Set('verilog_vlog_executable', 'vlog')
" See `$ vlog -h` for more options
call ale#Set('verilog_vlog_options', '-quiet -lint')

function! ale_linters#verilog#vlog#GetCommand(buffer) abort
    return '%e ' . ale#Pad(ale#Var(a:buffer, 'verilog_vlog_options')) . ' %t'
endfunction

function! ale_linters#verilog#vlog#Handle(buffer, lines) abort
    "Matches patterns like the following:
    "** Warning: add.v(7): (vlog-2623) Undefined variable: C.
    "** Error: file.v(1): (vlog-13294) Identifier must be declared with a port mode: C.
    let l:pattern = '^**\s\(\w*\):[a-zA-Z0-9\-\.\_\/ ]\+(\(\d\+\)):\s\+\(.*\)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[2] + 0,
        \   'type': l:match[1] is? 'Error' ? 'E' : 'W',
        \   'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('verilog', {
\   'name': 'vlog',
\   'output_stream': 'stdout',
\   'executable': {b -> ale#Var(b, 'verilog_vlog_executable')},
\   'command': function('ale_linters#verilog#vlog#GetCommand'),
\   'callback': 'ale_linters#verilog#vlog#Handle',
\})
