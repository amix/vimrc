" Author: Masahiro H https://github.com/mshr-h
" Description: iverilog for verilog files

call ale#Set('verilog_iverilog_options', '')

function! ale_linters#verilog#iverilog#GetCommand(buffer) abort
    return 'iverilog -t null -Wall '
    \   . ale#Var(a:buffer, 'verilog_iverilog_options')
    \   . ' %t'
endfunction

function! ale_linters#verilog#iverilog#Handle(buffer, lines) abort
    " Look for lines like the following.
    "
    " tb_me_top.v:37: warning: Instantiating module me_top with dangling input port 1 (rst_n) floating.
    " tb_me_top.v:17: syntax error
    " memory_single_port.v:2: syntax error
    " tb_me_top.v:17: error: Invalid module instantiation
    let l:pattern = '^[^:]\+:\(\d\+\): \(warning\|error\|syntax error\)\(: \(.\+\)\)\?'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:line = l:match[1] + 0
        let l:type = l:match[2] =~# 'error' ? 'E' : 'W'
        let l:text = l:match[2] is# 'syntax error' ? 'syntax error' : l:match[4]

        call add(l:output, {
        \   'lnum': l:line,
        \   'text': l:text,
        \   'type': l:type,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('verilog', {
\   'name': 'iverilog',
\   'output_stream': 'stderr',
\   'executable': 'iverilog',
\   'command': function('ale_linters#verilog#iverilog#GetCommand'),
\   'callback': 'ale_linters#verilog#iverilog#Handle',
\})
