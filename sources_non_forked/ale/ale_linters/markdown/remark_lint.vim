scriptencoding utf-8
" Author rhysd https://rhysd.github.io/, Dirk Roorda (dirkroorda), Adrián González Rus (@adrigzr)
" Description: remark-lint for Markdown files
call ale#Set('markdown_remark_lint_executable', 'remark')
call ale#Set('markdown_remark_lint_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('markdown_remark_lint_options', '')

function! ale_linters#markdown#remark_lint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'markdown_remark_lint_options')

    return '%e' . ale#Pad(l:options) . ' --no-stdout --no-color'
endfunction

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
\   'name': 'remark_lint',
\   'aliases': ['remark-lint'],
\   'executable_callback': ale#node#FindExecutableFunc('markdown_remark_lint', [
\       'node_modules/.bin/remark',
\   ]),
\   'command_callback': 'ale_linters#markdown#remark_lint#GetCommand',
\   'callback': 'ale_linters#markdown#remark_lint#Handle',
\   'output_stream': 'stderr',
\})
