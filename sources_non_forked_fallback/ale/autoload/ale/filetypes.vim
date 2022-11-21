" Author: w0rp <devw0rp@gmail.com>
" Description: This file handles guessing file extensions for filetypes, etc.

function! ale#filetypes#LoadExtensionMap() abort
    " Output includes:
    "    '*.erl setf erlang'
    let l:output = execute('exec "autocmd"')

    let l:map = {}

    for l:line in split(l:output, "\n")
        " Parse filetypes, like so:
        "
        "    *.erl setf erlang
        " *.md      set filetype=markdown
        " *.snippet setlocal filetype=snippets
        let l:match = matchlist(l:line, '\v^ *\*(\.[^ ]+).*set(f *| *filetype=|local *filetype=)([^ ]+)')

        if !empty(l:match)
            let l:map[substitute(l:match[3], '^=', '', '')] = l:match[1]
        endif
    endfor

    return l:map
endfunction

let s:cached_map = {}

function! s:GetCachedExtensionMap() abort
    if empty(s:cached_map)
        let s:cached_map = ale#filetypes#LoadExtensionMap()
    endif

    return s:cached_map
endfunction

function! ale#filetypes#GuessExtension(filetype) abort
    let l:map = s:GetCachedExtensionMap()
    let l:ext = get(l:map, a:filetype, '')

    " If we have an exact match, like something for javascript.jsx, use that.
    if !empty(l:ext)
        return l:ext
    endif

    " If we don't have an exact match, use the first filetype in the compound
    " filetype.
    for l:part in split(a:filetype, '\.')
        let l:ext = get(l:map, l:part, '')

        if !empty(l:ext)
            return l:ext
        endif
    endfor

    " Return an empty string if we don't find anything.
    return ''
endfunction
