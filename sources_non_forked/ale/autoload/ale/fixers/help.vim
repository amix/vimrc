" Author: w0rp <devw0rp@gmail.com>
" Description: Generic fixer functions for Vim help documents.

function! ale#fixers#help#AlignTags(buffer, lines) abort
    let l:new_lines = []

    for l:line in a:lines
        if len(l:line) != 79
            let l:match = matchlist(l:line, '\v +(\*[^*]+\*)$')

            if !empty(l:match)
                let l:start = l:line[:-len(l:match[0]) - 1]
                let l:tag = l:match[1]
                let l:spaces = repeat(' ', 79 - len(l:start) - len(l:tag))

                let l:line = l:start . l:spaces . l:tag
            endif
        endif

        call add(l:new_lines, l:line)
    endfor

    return l:new_lines
endfunction
