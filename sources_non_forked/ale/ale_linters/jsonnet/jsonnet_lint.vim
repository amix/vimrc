" Author: Trevor Whitney <trevorjwhitney@gmail.com>
" Description: jsonnet-lint for jsonnet files

call ale#Set('jsonnet_jsonnet_lint_executable', 'jsonnet-lint')
call ale#Set('jsonnet_jsonnet_lint_options', '')

function! ale_linters#jsonnet#jsonnet_lint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'jsonnet_jsonnet_lint_options')

    return '%e'
    \   . ale#Pad(l:options)
    \   . ' %t'
endfunction


function! ale_linters#jsonnet#jsonnet_lint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " ERROR: foo.jsonnet:22:3-12 expected token OPERATOR but got (IDENTIFIER, "bar")
    " ERROR: hoge.jsonnet:20:3 unexpected: "}" while parsing terminal
    " ERROR: main.jsonnet:212:1-14 Expected , or ; but got (IDENTIFIER, "older_cluster")
    let l:pattern = '^ERROR: [^:]*:\(\d\+\):\(\d\+\)\(-\d\+\)* \(.*\)'
    let l:output = []

    for l:line in a:lines
        let l:match = matchlist(l:line, l:pattern)

        if len(l:match) == 0
            continue
        endif

        let line_number = l:match[1] + 0
        let column = l:match[2] + 0
        " l:match[3] has optional -14, when linter is showing a range
        let text = l:match[4]


        " vcol is Needed to indicate that the column is a character.
        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'lnum': line_number,
        \   'vcol': 0,
        \   'col': column,
        \   'text': text,
        \   'type': 'E',
        \   'nr': -1,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('jsonnet', {
\   'name': 'jsonnet_lint',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'jsonnet_jsonnet_lint_executable')},
\   'command': function('ale_linters#jsonnet#jsonnet_lint#GetCommand'),
\   'callback': 'ale_linters#jsonnet#jsonnet_lint#Handle',
\})
