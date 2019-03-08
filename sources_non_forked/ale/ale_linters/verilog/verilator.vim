" Author: Masahiro H https://github.com/mshr-h
" Description: verilator for verilog files

" Set this option to change Verilator lint options
if !exists('g:ale_verilog_verilator_options')
    let g:ale_verilog_verilator_options = ''
endif

function! ale_linters#verilog#verilator#GetCommand(buffer) abort
    let l:filename = ale#util#Tempname() . '_verilator_linted.v'

    " Create a special filename, so we can detect it in the handler.
    call ale#command#ManageFile(a:buffer, l:filename)
    let l:lines = getbufline(a:buffer, 1, '$')
    call ale#util#Writefile(a:buffer, l:lines, l:filename)

    return 'verilator --lint-only -Wall -Wno-DECLFILENAME '
    \   . ale#Var(a:buffer, 'verilog_verilator_options') .' '
    \   . ale#Escape(l:filename)
endfunction

function! ale_linters#verilog#verilator#Handle(buffer, lines) abort
    " Look for lines like the following.
    "
    " %Error: addr_gen.v:3: syntax error, unexpected IDENTIFIER
    " %Warning-WIDTH: addr_gen.v:26: Operator ASSIGNDLY expects 12 bits on the Assign RHS, but Assign RHS's CONST '20'h0' generates 20 bits.
    " %Warning-UNUSED: test.v:3: Signal is not used: a
    " %Warning-UNDRIVEN: test.v:3: Signal is not driven: clk
    " %Warning-UNUSED: test.v:4: Signal is not used: dout
    " %Warning-BLKSEQ: test.v:10: Blocking assignments (=) in sequential (flop or latch) block; suggest delayed assignments (<=).
    let l:pattern = '^%\(Warning\|Error\)[^:]*:\([^:]\+\):\(\d\+\): \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:line = l:match[3] + 0
        let l:type = l:match[1] is# 'Error' ? 'E' : 'W'
        let l:text = l:match[4]
        let l:file = l:match[2]

        if l:file =~# '_verilator_linted.v'
            call add(l:output, {
            \   'lnum': l:line,
            \   'text': l:text,
            \   'type': l:type,
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('verilog', {
\   'name': 'verilator',
\   'output_stream': 'stderr',
\   'executable': 'verilator',
\   'command': function('ale_linters#verilog#verilator#GetCommand'),
\   'callback': 'ale_linters#verilog#verilator#Handle',
\   'read_buffer': 0,
\})
