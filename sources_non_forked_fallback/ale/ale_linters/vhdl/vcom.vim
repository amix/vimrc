" Author:      John Gentile <johncgentile17@gmail.com>
" Description: Adds support for Mentor Graphics Questa/ModelSim `vcom` VHDL compiler/checker

call ale#Set('vhdl_vcom_executable', 'vcom')
" Use VHDL-2008. See `$ vcom -h` for more options
call ale#Set('vhdl_vcom_options', '-2008 -quiet -lint')

function! ale_linters#vhdl#vcom#GetCommand(buffer) abort
    return '%e ' . ale#Pad(ale#Var(a:buffer, 'vhdl_vcom_options')) . ' %t'
endfunction

function! ale_linters#vhdl#vcom#Handle(buffer, lines) abort
    "Matches patterns like the following:
    "** Warning: ../path/to/file.vhd(218): (vcom-1236) Shared variables must be of a protected type.
    "** Error: tb_file.vhd(73): (vcom-1136) Unknown identifier "aresetn".
    "** Error: tb_file.vhd(73): Bad resolution function (STD_LOGIC) for type (error).
    "** Error: tb_file.vhd(73): near ":": (vcom-1576) expecting ';' or ')'.
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

call ale#linter#Define('vhdl', {
\   'name': 'vcom',
\   'output_stream': 'stdout',
\   'executable': {b -> ale#Var(b, 'vhdl_vcom_executable')},
\   'command': function('ale_linters#vhdl#vcom#GetCommand'),
\   'callback': 'ale_linters#vhdl#vcom#Handle',
\})
