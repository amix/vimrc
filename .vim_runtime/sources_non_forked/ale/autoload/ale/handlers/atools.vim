" Author: Leo <thinkabit.ukim@gmail.com>
" Description: Handlers for output expected from atools

function! ale#handlers#atools#Handle(buffer, lines) abort
    " Format: SEVERITY:[TAG]:PATH:LINENUM:MSG
    " Example: MC:[AL5]:./APKBUILD:12:variable set to empty string: install=
    let l:pattern = '\([^:]\+\):\([^:]\+\):\([^:]\+\):\(\d\+\):\(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        " We are expected to receive 2 characters, the first character
        " can be 'S', 'I', 'M' 'T', which are respectively:
        " Serious (Error)
        " Important (Error)
        " Minor (Warning)
        " Style (Warning)
        "
        " The second character can be either 'C' or 'P', which are respectively:
        " Certain (Error)
        " Possible (Warning)
        let l:severity = matchstr(l:match[1], '^.')
        let l:certainty = matchstr(l:match[1], '.$')

        let l:type = 'E'
        " If the tag returns 'Minor' or 'Style' or is 'Possible'
        " then return a warning

        if l:severity is# 'M' || l:severity is# 'T' || l:certainty is# 'P'
            let l:type = 'W'
        endif

        call add(l:output, {
        \    'lnum': l:match[4] + 0,
        \    'text': l:match[5],
        \    'type': l:type,
        \    'code': matchstr(l:match[2], 'AL[0-9]*'),
        \})
    endfor

    return l:output
endfunction
