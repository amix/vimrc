scriptencoding utf-8
" Description: This file defines a handler function which ought to work for
" any program which outputs errors in the format that ponyc uses.

function! s:RemoveUnicodeQuotes(text) abort
    let l:text = a:text
    let l:text = substitute(l:text, '[`´‘’]', '''', 'g')
    let l:text = substitute(l:text, '\v\\u2018([^\\]+)\\u2019', '''\1''', 'g')
    let l:text = substitute(l:text, '[“”]', '"', 'g')

    return l:text
endfunction

function! ale#handlers#pony#HandlePonycFormat(buffer, lines) abort
    " Look for lines like the following.
    " /home/code/pony/classes/Wombat.pony:22:30: can't lookup private fields from outside the type

    let l:pattern = '\v^([^:]+):(\d+):(\d+)?:? (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:item = {
        \   'filename': l:match[1],
        \   'lnum': str2nr(l:match[2]),
        \   'col': str2nr(l:match[3]),
        \   'type': 'E',
        \   'text': s:RemoveUnicodeQuotes(l:match[4]),
        \}

        call add(l:output, l:item)
    endfor

    return l:output
endfunction
