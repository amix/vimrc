" Author: w0rp <devw0rp@gmail.com>
" Description: Generic functions for fixing files with.

function! ale#fixers#generic#RemoveTrailingBlankLines(buffer, lines) abort
    let l:end_index = len(a:lines) - 1

    while l:end_index > 0 && empty(a:lines[l:end_index])
        let l:end_index -= 1
    endwhile

    return a:lines[:l:end_index]
endfunction

" Remove all whitespaces at the end of lines
function! ale#fixers#generic#TrimWhitespace(buffer, lines) abort
    let l:index = 0
    let l:lines_new = range(len(a:lines))

    for l:line in a:lines
        let l:lines_new[l:index] = substitute(l:line, '\s\+$', '', 'g')
        let l:index = l:index + 1
    endfor

    return l:lines_new
endfunction
