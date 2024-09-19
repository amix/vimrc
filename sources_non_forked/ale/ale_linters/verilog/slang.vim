" Author: Alvin Rolling <alvinrolling@gmail.com>
" Description: slang for verilog files

" Set this option to change Slang lint options
if !exists('g:ale_verilog_slang_options')
    let g:ale_verilog_slang_options = ''
endif

" --lint-only
function! ale_linters#verilog#slang#GetCommand(buffer) abort
    return 'slang -Weverything '
    \   . '-I%s:h '
    \   . ale#Var(a:buffer, 'verilog_slang_options') .' '
    \   . '%t'
endfunction

function! s:RemoveUnicodeQuotes(text) abort
    let l:text = a:text
    let l:text = substitute(l:text, '[`´‘’]', '''', 'g')
    let l:text = substitute(l:text, '[“”]', '"', 'g')

    return l:text
endfunction

function! ale_linters#verilog#slang#Handle(buffer, lines) abort
    let l:pattern = '\v^([a-zA-Z]?:?[^:]+):(\d+)?:?(\d+)?:? ([^:]+): (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:item = {
        \   'lnum': str2nr(l:match[2]),
        \   'type': (l:match[4] is# 'error') ? 'E' : 'W',
        \   'text': s:RemoveUnicodeQuotes(l:match[5]),
        \}

        if !empty(l:match[3])
            let l:item.col = str2nr(l:match[3])
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('verilog', {
\   'name': 'slang',
\   'output_stream': 'stderr',
\   'executable': 'slang',
\   'command': function('ale_linters#verilog#slang#GetCommand'),
\   'callback': 'ale_linters#verilog#slang#Handle',
\   'read_buffer': 0,
\})
