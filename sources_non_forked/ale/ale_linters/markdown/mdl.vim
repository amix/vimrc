" Author: Steve Dignam <steve@dignam.xyz>, Josh Leeb-du Toit <joshleeb.com>
" Description: Support for mdl, a markdown linter.

call ale#Set('markdown_mdl_executable', 'mdl')
call ale#Set('markdown_mdl_options', '')

function! ale_linters#markdown#mdl#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'markdown_mdl_executable')
endfunction

function! ale_linters#markdown#mdl#GetCommand(buffer) abort
    let l:executable = ale_linters#markdown#mdl#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'bundle$'
    \   ? ' exec mdl'
    \   : ''

    let l:options = ale#Var(a:buffer, 'markdown_mdl_options')

    return ale#Escape(l:executable) . l:exec_args
    \   . (!empty(l:options) ? ' ' . l:options : '')
endfunction

function! ale_linters#markdown#mdl#Handle(buffer, lines) abort
    " matches: '(stdin):173: MD004 Unordered list style'
    let l:pattern = ':\(\d*\): \(.*\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'text': l:match[2],
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('markdown', {
\   'name': 'mdl',
\   'executable': function('ale_linters#markdown#mdl#GetExecutable'),
\   'command': function('ale_linters#markdown#mdl#GetCommand'),
\   'callback': 'ale_linters#markdown#mdl#Handle'
\})
