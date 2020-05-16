" Author: Keith Maxwell <keith.maxwell@gmail.com>
" Description: terraform fmt to check for errors

call ale#Set('terraform_terraform_executable', 'terraform')

function! ale_linters#terraform#terraform#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'terraform_terraform_executable')
endfunction

function! ale_linters#terraform#terraform#GetCommand(buffer) abort
    return ale#Escape(ale_linters#terraform#terraform#GetExecutable(a:buffer))
    \   . ' fmt -no-color --check=true -'
endfunction

function! ale_linters#terraform#terraform#Handle(buffer, lines) abort
    let l:head = '^Error running fmt: In <standard input>: '
    let l:output = []
    let l:patterns = [
    \   l:head.'At \(\d\+\):\(\d\+\): \(.*\)$',
    \   l:head.'\(.*\)$'
    \]

    for l:match in ale#util#GetMatches(a:lines, l:patterns)
        if len(l:match[2]) > 0
            call add(l:output, {
            \   'lnum': str2nr(l:match[1]),
            \   'col': str2nr(l:match[2]),
            \   'text': l:match[3],
            \   'type': 'E',
            \})
        else
            call add(l:output, {
            \   'lnum': line('$'),
            \   'text': l:match[1],
            \   'type': 'E',
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('terraform', {
\   'name': 'terraform',
\   'output_stream': 'stderr',
\   'executable': function('ale_linters#terraform#terraform#GetExecutable'),
\   'command': function('ale_linters#terraform#terraform#GetCommand'),
\   'callback': 'ale_linters#terraform#terraform#Handle',
\})
