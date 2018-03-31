" Author rhysd https://rhysd.github.io/, Dirk Roorda (dirkroorda), Adrián González Rus (@adrigzr)
" Description: remark-lint for Markdown files

function! ale_linters#markdown#remark_lint#Handle(buffer, lines) abort
    " matches: '  1:4  warning  Incorrect list-item indent: add 1 space  list-item-indent  remark-lint'
    " matches: '  18:71-19:1  error  Missing new line after list item  list-item-spacing  remark-lint',
    let l:pattern = '^ \+\(\d\+\):\(\d\+\)\(-\(\d\+\):\(\d\+\)\)\?  \(warning\|error\)  \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:item = {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': l:match[6] is# 'error' ? 'E' : 'W',
        \   'text': l:match[7],
        \}
        if l:match[3] isnot# ''
            let l:item.end_lnum = l:match[4] + 0
            let l:item.end_col = l:match[5] + 0
        endif
        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('markdown', {
\   'name': 'remark-lint',
\   'executable': 'remark',
\   'command': 'remark --no-stdout --no-color %s',
\   'callback': 'ale_linters#markdown#remark_lint#Handle',
\   'lint_file': 1,
\   'output_stream': 'stderr',
\})
