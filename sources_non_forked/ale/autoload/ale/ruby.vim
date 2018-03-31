" Author: Eddie Lebow https://github.com/elebow
" Description: Functions for integrating with Ruby tools

" Find the nearest dir contining "app", "db", and "config", and assume it is
" the root of a Rails app.
function! ale#ruby#FindRailsRoot(buffer) abort
    for l:name in ['app', 'config', 'db']
        let l:dir = fnamemodify(
        \   ale#path#FindNearestDirectory(a:buffer, l:name),
        \   ':h:h'
        \)

        if l:dir isnot# '.'
        \&& isdirectory(l:dir . '/app')
        \&& isdirectory(l:dir . '/config')
        \&& isdirectory(l:dir . '/db')
            return l:dir
        endif
    endfor

    return ''
endfunction
