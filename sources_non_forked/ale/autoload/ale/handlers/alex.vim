" Author: Johannes Wienke <languitar@semipol.de>
" Description: Error handling for errors in alex output format

function! ale#handlers#alex#Handle(buffer, lines) abort
    " Example output:
    "       6:256-6:262  warning  Be careful with “killed”, it’s profane in some cases      killed           retext-profanities
    let l:pattern = '^ *\(\d\+\):\(\d\+\)-\(\d\+\):\(\d\+\) \+warning \+\(.\{-\}\)  \+\(.\{-\}\)  \+\(.\{-\}\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'end_lnum': l:match[3] + 0,
        \   'end_col': l:match[4] - 1,
        \   'text': l:match[5] . ' (' . (l:match[7]) . ')',
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction
