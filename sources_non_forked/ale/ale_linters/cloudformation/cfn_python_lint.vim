" Author: Yasuhiro Kiyota <yasuhiroki.duck@gmail.com>
" Description: Support cfn-python-lint for AWS Cloudformation template file

function! ale_linters#cloudformation#cfn_python_lint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " sample.template.yaml:96:7:96:15:E3012:Property Resources/Sample/Properties/FromPort should be of type Integer
    let l:pattern = '\v^(.*):(\d+):(\d+):(\d+):(\d+):([[:alnum:]]+):(.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:code = l:match[6]

        if ale#path#IsBufferPath(a:buffer, l:match[1])
            call add(l:output, {
            \   'lnum': l:match[2],
            \   'col': l:match[3],
            \   'end_lnum': l:match[4],
            \   'end_col': l:match[5],
            \   'code': l:code,
            \   'type': l:code[:0] is# 'E' ? 'E' : 'W',
            \   'text': l:match[7]
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('cloudformation', {
\   'name': 'cloudformation',
\   'executable': 'cfn-lint',
\   'command': 'cfn-lint --template %t --format parseable',
\   'callback': 'ale_linters#cloudformation#cfn_python_lint#Handle',
\})
