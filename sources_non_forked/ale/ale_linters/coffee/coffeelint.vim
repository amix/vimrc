" Author: Prashanth Chandra https://github.com/prashcr
" Description: coffeelint linter for coffeescript files

function! ale_linters#coffee#coffeelint#GetExecutable(buffer) abort
    return ale#path#ResolveLocalPath(
    \   a:buffer,
    \   'node_modules/.bin/coffeelint',
    \   'coffeelint'
    \)
endfunction

function! ale_linters#coffee#coffeelint#GetCommand(buffer) abort
    return ale_linters#coffee#coffeelint#GetExecutable(a:buffer)
    \   . ' --stdin --reporter csv'
endfunction

function! ale_linters#coffee#coffeelint#Handle(buffer, lines) abort
    " Matches patterns like the following:
    "
    " path,lineNumber,lineNumberEnd,level,message
    " stdin,14,,error,Throwing strings is forbidden
    "
    " Note that we currently ignore lineNumberEnd for multiline errors
    let l:pattern = 'stdin,\(\d\+\),\(\d*\),\(.\{-1,}\),\(.\+\)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': str2nr(l:match[1]),
        \   'type': l:match[3] is# 'error' ? 'E' : 'W',
        \   'text': l:match[4],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('coffee', {
\   'name': 'coffeelint',
\   'executable': function('ale_linters#coffee#coffeelint#GetExecutable'),
\   'command': function('ale_linters#coffee#coffeelint#GetCommand'),
\   'callback': 'ale_linters#coffee#coffeelint#Handle',
\})
