" Author:      John Gentile <johncgentile17@gmail.com>
" Description: Adds support for Xilinx Vivado `xvlog` Verilog compiler/checker

call ale#Set('verilog_xvlog_executable', 'xvlog')
call ale#Set('verilog_xvlog_options', '')

function! ale_linters#verilog#xvlog#GetCommand(buffer) abort
    return '%e ' . ale#Pad(ale#Var(a:buffer, 'verilog_xvlog_options')) . ' %t'
endfunction

function! ale_linters#verilog#xvlog#Handle(buffer, lines) abort
    "Matches patterns like the following:
    " ERROR: [VRFC 10-1412] syntax error near output [/path/to/file.v:5]
    let l:pattern = '^ERROR:\s\+\(\[.*\)\[.*:\([0-9]\+\)\]'
    let l:output = []

    " NOTE: `xvlog` only prints 'INFO' and 'ERROR' messages
    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[2] + 0,
        \   'type': 'E',
        \   'text': l:match[1],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('verilog', {
\   'name': 'xvlog',
\   'output_stream': 'stdout',
\   'executable': {b -> ale#Var(b, 'verilog_xvlog_executable')},
\   'command': function('ale_linters#verilog#xvlog#GetCommand'),
\   'callback': 'ale_linters#verilog#xvlog#Handle',
\})
