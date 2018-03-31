" Author: KabbAmine <amine.kabb@gmail.com>

let g:ale_yaml_yamllint_executable =
\   get(g:, 'ale_yaml_yamllint_executable', 'yamllint')

let g:ale_yaml_yamllint_options =
\   get(g:, 'ale_yaml_yamllint_options', '')

function! ale_linters#yaml#yamllint#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'yaml_yamllint_executable')
endfunction

function! ale_linters#yaml#yamllint#GetCommand(buffer) abort
    return ale_linters#yaml#yamllint#GetExecutable(a:buffer)
    \   . ' ' . ale#Var(a:buffer, 'yaml_yamllint_options')
    \   . ' -f parsable %t'
endfunction

function! ale_linters#yaml#yamllint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    " something.yaml:1:1: [warning] missing document start "---" (document-start)
    " something.yml:2:1: [error] syntax error: expected the node content, but found '<stream end>'
    let l:pattern = '^.*:\(\d\+\):\(\d\+\): \[\(error\|warning\)\] \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:line = l:match[1] + 0
        let l:col = l:match[2] + 0
        let l:type = l:match[3]
        let l:text = l:match[4]

        call add(l:output, {
        \   'lnum': l:line,
        \   'col': l:col,
        \   'text': l:text,
        \   'type': l:type is# 'error' ? 'E' : 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('yaml', {
\   'name': 'yamllint',
\   'executable_callback': 'ale_linters#yaml#yamllint#GetExecutable',
\   'command_callback': 'ale_linters#yaml#yamllint#GetCommand',
\   'callback': 'ale_linters#yaml#yamllint#Handle',
\})
