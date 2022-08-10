" Author: Alex McKinney <alexmckinney01@gmail.com>
" Description: Run buf format.

call ale#Set('proto_buf_format_executable', 'buf')

function! ale#fixers#buf_format#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'proto_buf_format_executable')

    return {
    \   'command': ale#Escape(l:executable) . ' format %t',
    \}
endfunction
