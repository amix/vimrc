" Author: Dawid Kurek https://github.com/dawikur
" Description: Handle errors for cpplint.

function! ale#handlers#cpplint#HandleCppLintFormat(buffer, lines) abort
    " Look for lines like the following.
    " test.cpp:5:  Estra space after ( in function call [whitespace/parents] [4]
    let l:pattern = '^.\{-}:\(\d\+\): *\(.\+\) *\[\(.*/.*\)\] '
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': 0,
        \   'text': join(split(l:match[2])),
        \   'code': l:match[3],
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction
