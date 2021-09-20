" Author: Nathan Sharp <nwsharp+eda@live.com>
" Description: Yosys for Verilog files

call ale#Set('verilog_yosys_executable', 'yosys')
call ale#Set('verilog_yosys_options', '-Q -T -p ''read_verilog %s''')

function! ale_linters#verilog#yosys#GetCommand(buffer) abort
    return '%e ' . ale#Var(a:buffer, 'verilog_yosys_options') . ' 2>&1'
endfunction

function! ale_linters#verilog#yosys#Handle(buffer, lines) abort
    let l:output = []
    let l:path = fnamemodify(bufname(a:buffer), ':p')

    for l:match in ale#util#GetMatches(a:lines, '^\([^:]\+\):\(\d\+\): \(WARNING\|ERROR\): \(.\+\)$')
        call add(l:output, {
        \   'lnum': str2nr(l:match[2]),
        \   'text': l:match[4],
        \   'type': l:match[3][0],
        \   'filename': l:match[1],
        \})
    endfor

    for l:match in ale#util#GetMatches(a:lines, '^\(Warning\|ERROR\): \(.\+\)$')
        call add(l:output, {
        \   'lnum': 1,
        \   'text': l:match[2],
        \   'type': l:match[1][0],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('verilog', {
\   'name': 'yosys',
\   'output_stream': 'stdout',
\   'executable': {b -> ale#Var(b, 'verilog_yosys_executable')},
\   'command': function('ale_linters#verilog#yosys#GetCommand'),
\   'callback': 'ale_linters#verilog#yosys#Handle',
\   'lint_file': 1,
\})
