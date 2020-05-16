" Author:      John Gentile <johncgentile17@gmail.com>
" Description: Adds support for Xilinx Vivado `xvhdl` VHDL compiler/checker

call ale#Set('vhdl_xvhdl_executable', 'xvhdl')
" Use VHDL-2008. See `$ xvhdl -h` for more options
call ale#Set('vhdl_xvhdl_options', '--2008')

function! ale_linters#vhdl#xvhdl#GetCommand(buffer) abort
    return '%e ' . ale#Pad(ale#Var(a:buffer, 'vhdl_xvhdl_options')) . ' %t'
endfunction

function! ale_linters#vhdl#xvhdl#Handle(buffer, lines) abort
    "Matches patterns like the following:
    " ERROR: [VRFC 10-91] aresetn is not declared [/path/to/file.vhd:17]
    " ERROR: [VRFC 10-91] m_axis_tx_tdata is not declared [/home/user/tx_data.vhd:128]
    let l:pattern = '^ERROR:\s\+\(\[.*\)\[.*:\([0-9]\+\)\]'
    let l:output = []

    " NOTE: `xvhdl` only prints 'INFO' and 'ERROR' messages
    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[2] + 0,
        \   'type': 'E',
        \   'text': l:match[1],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('vhdl', {
\   'name': 'xvhdl',
\   'output_stream': 'stdout',
\   'executable': {b -> ale#Var(b, 'vhdl_xvhdl_executable')},
\   'command': function('ale_linters#vhdl#xvhdl#GetCommand'),
\   'callback': 'ale_linters#vhdl#xvhdl#Handle',
\})
