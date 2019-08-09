" Author: KabbAmine - https://github.com/KabbAmine
" Description: Coffee for checking coffee files

function! ale_linters#coffee#coffee#GetExecutable(buffer) abort
    return ale#path#ResolveLocalPath(
    \   a:buffer,
    \   'node_modules/.bin/coffee',
    \   'coffee'
    \)
endfunction

function! ale_linters#coffee#coffee#GetCommand(buffer) abort
    return ale_linters#coffee#coffee#GetExecutable(a:buffer)
    \   . ' -cp -s'
endfunction

call ale#linter#Define('coffee', {
\   'name': 'coffee',
\   'executable': function('ale_linters#coffee#coffee#GetExecutable'),
\   'command': function('ale_linters#coffee#coffee#GetCommand'),
\   'output_stream': 'stderr',
\   'callback': 'ale#handlers#gcc#HandleGCCFormat',
\})
