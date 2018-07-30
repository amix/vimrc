" Author: nametake https://nametake.github.io
" Description: apiblueprint parser

function! ale_linters#apiblueprint#drafter#HandleErrors(buffer, lines) abort
    " Matches patterns line the following:
    "
    " warning: (3)  unable to parse response signature, expected 'response [<HTTP status code>] [(<media type>)]'; line 4, column 3k - line 4, column 22
    " warning: (10)  message-body asset is expected to be a pre-formatted code block, separate it by a newline and indent every of its line by 12 spaces or 3 tabs; line 30, column 5 - line 30, column 9; line 31, column 9 - line 31, column 14; line 32, column 9 - line 32, column 14
    let l:pattern = '\(^.*\): (\d\+)  \(.\{-\}\); line \(\d\+\), column \(\d\+\) - line \d\+, column \d\+\(.*; line \d\+, column \d\+ - line \(\d\+\), column \(\d\+\)\)\{-\}$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines[2:], l:pattern)
        let l:item = {
        \   'type': l:match[1] is# 'warning' ? 'W' : 'E',
        \   'text': l:match[2],
        \   'lnum': l:match[3] + 0,
        \   'col': l:match[4] + 0,
        \}
        if l:match[5] isnot# ''
            let l:item.end_lnum = l:match[6] + 0
            let l:item.end_col = l:match[7] + 0
        endif
        call add(l:output, l:item)
    endfor

    return l:output
endfunction


call ale#linter#Define('apiblueprint', {
\   'name': 'drafter',
\   'output_stream': 'stderr',
\   'executable': 'drafter',
\   'command': 'drafter --use-line-num --validate',
\   'callback': 'ale_linters#apiblueprint#drafter#HandleErrors',
\})
