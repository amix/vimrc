" Author: 0xhyoga <0xhyoga@gmx.com>,
" Description: This file implements handlers specific to Cairo
"
function! ale#handlers#cairo#HandleCairoErrors(buffer, lines) abort
    " Matches patterns like the following:
    " Error: Expected ';' but got '('
    "    --> /path/to/file/file.cairo:1:10:)
    let l:pattern = '\v(error|warning): (.*)$'
    let l:line_and_column_pattern = '\v\.cairo:(\d+):(\d+)'
    let l:exclude_pattern = '\vcould not compile.*'
    let l:output = []

    for l:line in a:lines
        let l:match = matchlist(l:line, l:pattern)

        if len(l:match) == 0
            let l:match = matchlist(l:line, l:line_and_column_pattern)

            if len(l:match) > 0
                let l:index = len(l:output) - 1
                let l:output[l:index]['lnum'] = l:match[1] + 0
                let l:output[l:index]['col'] = l:match[2] + 0
            endif
        else
            let l:text = l:match[2]

            if l:text !~# l:exclude_pattern
                let l:isError = l:match[1] is? 'Error'

                call add(l:output, {
                \   'lnum': 0,
                \   'col': 0,
                \   'text': l:text,
                \   'type': l:isError ? 'E' : 'W',
                \})
            endif
        endif
    endfor

    return l:output
endfunction
