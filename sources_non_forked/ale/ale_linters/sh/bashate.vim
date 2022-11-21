" Author: hsanson <hsanson@gmail.com>
" Description: Lints sh files using bashate
" URL: https://github.com/openstack/bashate

call ale#Set('sh_bashate_executable', 'bashate')
call ale#Set('sh_bashate_options', '')

function! ale_linters#sh#bashate#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'sh_bashate_executable')
endfunction

function! ale_linters#sh#bashate#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'sh_bashate_options')
    let l:executable = ale_linters#sh#bashate#GetExecutable(a:buffer)

    return ale#Escape(l:executable) . ' ' . l:options . ' ' . '%t'
endfunction

function! ale_linters#sh#bashate#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " /path/to/script/file:694:1: E003 Indent not multiple of 4
    let l:pattern = ':\(\d\+\):\(\d\+\): \(.*\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': str2nr(l:match[1]),
        \   'col': str2nr(l:match[2]),
        \   'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('sh', {
\   'name': 'bashate',
\   'output_stream': 'stdout',
\   'executable': function('ale_linters#sh#bashate#GetExecutable'),
\   'command': function('ale_linters#sh#bashate#GetCommand'),
\   'callback': 'ale_linters#sh#bashate#Handle',
\})
