" Author: Chuck Grindel <chuck.grindel@gmail.com>
" Description: Bazel Starlark lint support using buildifier.

function! ale_linters#bzl#buildifier#GetCommand(buffer) abort
    let l:executable = ale#Escape(ale#fixers#buildifier#GetExecutable(a:buffer))
    let l:options = ale#Var(a:buffer, 'bazel_buildifier_options')
    let l:filename = ale#Escape(bufname(a:buffer))

    let l:command = l:executable . ' -mode check -lint warn -path %s'

    if l:options isnot# ''
        let l:command .= ' ' . l:options
    endif

    return l:command
endfunction

function! ale_linters#bzl#buildifier#Handle(buffer, lines) abort
    let l:pattern = '\v^[^:]+:(\d+):(\d+)?:?\s+(syntax error near)?(.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[3] . l:match[4],
        \   'type': l:match[3] is# 'syntax error near' ? 'E' : 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('bzl', {
\   'name': 'buildifier',
\   'output_stream': 'both',
\   'executable': function('ale#fixers#buildifier#GetExecutable'),
\   'command': function('ale_linters#bzl#buildifier#GetCommand'),
\   'callback': function('ale_linters#bzl#buildifier#Handle'),
\})
