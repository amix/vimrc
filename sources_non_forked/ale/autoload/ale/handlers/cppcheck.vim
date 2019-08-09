" Description: Handle errors for cppcheck.

function! ale#handlers#cppcheck#HandleCppCheckFormat(buffer, lines) abort
    " Look for lines like the following.
    "
    " [test.cpp:5]: (error) Array 'a[10]' accessed at index 10, which is out of bounds
    let l:pattern = '\v^\[(.+):(\d+)\]: \(([a-z]+)\) (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        if ale#path#IsBufferPath(a:buffer, l:match[1])
            call add(l:output, {
            \   'lnum': str2nr(l:match[2]),
            \   'type': l:match[3] is# 'error' ? 'E' : 'W',
            \   'text': l:match[4],
            \})
        endif
    endfor

    return l:output
endfunction
