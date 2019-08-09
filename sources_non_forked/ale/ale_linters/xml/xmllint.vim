" Author: q12321q <q12321q@gmail.com>
" Description: This file adds support for checking XML code with xmllint.

" CLI options
let g:ale_xml_xmllint_executable = get(g:, 'ale_xml_xmllint_executable', 'xmllint')
let g:ale_xml_xmllint_options = get(g:, 'ale_xml_xmllint_options', '')

function! ale_linters#xml#xmllint#GetCommand(buffer) abort
    return '%e'
    \   . ale#Pad(ale#Var(a:buffer, 'xml_xmllint_options'))
    \   . ' --noout -'
endfunction

function! ale_linters#xml#xmllint#Handle(buffer, lines) abort
    " Matches patterns lines like the following:
    " file/path:123: error level : error message
    let l:pattern_message = '\v^([^:]+):(\d+):\s*(([^:]+)\s*:\s+.*)$'

    " parse column token line like that:
    " file/path:123: parser error : Opening and ending tag mismatch: foo line 1 and bar
    " </bar>
    "       ^
    let l:pattern_column_token = '\v^\s*\^$'

    let l:output = []

    for l:line in a:lines
        " Parse error/warning lines
        let l:match_message = matchlist(l:line, l:pattern_message)

        if !empty(l:match_message)
            let l:line = l:match_message[2] + 0
            let l:type = l:match_message[4] =~? 'warning' ? 'W' : 'E'
            let l:text = l:match_message[3]

            call add(l:output, {
            \   'lnum': l:line,
            \   'text': l:text,
            \   'type': l:type,
            \})

            continue
        endif

        " Parse column position
        let l:match_column_token = matchlist(l:line, l:pattern_column_token)

        if !empty(l:output) && !empty(l:match_column_token)
            let l:previous = l:output[len(l:output) - 1]
            let l:previous['col'] = len(l:match_column_token[0])

            continue
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('xml', {
\   'name': 'xmllint',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'xml_xmllint_executable')},
\   'command': function('ale_linters#xml#xmllint#GetCommand'),
\   'callback': 'ale_linters#xml#xmllint#Handle',
\ })
