" Author: Samuel Branisa <branisa.samuel@icloud.com>
" Description: rflint linting for robot framework files

call ale#Set('robot_rflint_executable', 'rflint')

function! ale_linters#robot#rflint#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'robot_rflint_executable')
endfunction

function! ale_linters#robot#rflint#GetCommand(buffer) abort
    let l:executable = ale_linters#robot#rflint#GetExecutable(a:buffer)
    let l:flags = '--format'
    \ . ' "{filename}:{severity}:{linenumber}:{char}:{rulename}:{message}"'

    return l:executable
    \   . ' '
    \   . l:flags
    \   . ' %s'
endfunction

function! ale_linters#robot#rflint#Handle(buffer, lines) abort
    let l:pattern = '\v^([[:alnum:][:punct:]]+):(W|E):([[:digit:]]+):([[:digit:]]+):([[:alnum:]]+):(.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'filename': l:match[1],
        \   'type': l:match[2],
        \   'lnum': str2nr(l:match[3]),
        \   'col': str2nr(l:match[4]),
        \   'text': l:match[5],
        \   'detail': l:match[6],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('robot', {
\   'name': 'rflint',
\   'executable': function('ale_linters#robot#rflint#GetExecutable'),
\   'command': function('ale_linters#robot#rflint#GetCommand'),
\   'callback': 'ale_linters#robot#rflint#Handle',
\})

