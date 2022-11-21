" Author: Auri <me@aurieh.me>
" Description: Functions for integrating with D linters.

function! ale#d#FindDUBConfig(buffer) abort
    " Find a DUB configuration file in ancestor paths.
    " The most DUB-specific names will be tried first.
    for l:possible_filename in ['dub.sdl', 'dub.json', 'package.json']
        let l:dub_file = ale#path#FindNearestFile(a:buffer, l:possible_filename)

        if !empty(l:dub_file)
            return l:dub_file
        endif
    endfor

    return ''
endfunction
