scriptencoding utf-8
" Author: Christian Gibbons <cgibbons@gmu.edu>
" Description: This file defines a handler function that should work for the
" flawfinder format with the -CDQS flags.

" Swiped this function from the GCC handler. Not sure if needed, but doesn't
" hurt to have it.
function! s:RemoveUnicodeQuotes(text) abort
    let l:text = a:text
    let l:text = substitute(l:text, '[`´‘’]', '''', 'g')
    let l:text = substitute(l:text, '\v\\u2018([^\\]+)\\u2019', '''\1''', 'g')
    let l:text = substitute(l:text, '[“”]', '"', 'g')

    return l:text
endfunction

function! ale#handlers#flawfinder#HandleFlawfinderFormat(buffer, lines) abort
    " Look for lines like the following.
    "
    " <stdin>:12:4:  [2] (buffer) char:Statically-sized arrays can be improperly restricted, leading to potential overflows or other issues (CWE-119!/CWE-120).  Perform bounds checking, use functions that limit length, or ensure that the size is larger than the maximum possible length.
    " <stdin>:31:4:  [1] (buffer) strncpy:Easily used incorrectly; doesn't always \0-terminate or check for invalid pointers [MS-banned] (CWE-120).
    let l:pattern = '\v^([a-zA-Z]?:?[^:]+):(\d+):(\d+)?:? ( \[[0-5]\] [^:]+):(.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        " Use severity level to determine if it should be considered a warning
        " or error.
        let l:severity = str2nr(matchstr(split(l:match[4])[0], '[0-5]'))

        let l:item = {
        \   'lnum': str2nr(l:match[2]),
        \   'col': str2nr(l:match[3]),
        \   'type': (l:severity < ale#Var(a:buffer, 'c_flawfinder_error_severity'))
        \       ? 'W' : 'E',
        \   'text': s:RemoveUnicodeQuotes(join(split(l:match[4])[1:]) . ': ' . l:match[5]),
        \}

        " If the filename is something like <stdin>, <nofile> or -, then
        " this is an error for the file we checked.
        if l:match[1] isnot# '-' && l:match[1][0] isnot# '<'
            let l:item['filename'] = l:match[1]
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction
