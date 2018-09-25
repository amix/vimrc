" Author: Anthony DeDominic <adedomin@gmail.com>
" Description: Handle output from gawk's --lint option

function! ale#handlers#gawk#HandleGawkFormat(buffer, lines) abort
    " Look for lines like the following:
    " gawk: /tmp/v0fddXz/1/something.awk:1: ^ invalid char ''' in expression
    let l:pattern = '^.\{-}:\(\d\+\):\s\+\(warning:\|\^\)\s*\(.*\)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:ecode = 'E'

        if l:match[2] is? 'warning:'
            let l:ecode = 'W'
        endif

        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': 0,
        \   'text': l:match[3],
        \   'code': 0,
        \   'type': l:ecode,
        \})
    endfor

    return l:output
endfunction
