scriptencoding utf-8LE
" Description: This file defines a handler function for linting OpenSCAD files
" with SCA2D

function! ale#handlers#openscad#SCA2D_callback(buffer, lines) abort
    " Example output::
    " foo.scad:3:1: W2001: Variable `unused` overwritten within scope.
    " foo.scad:1:1: F0001: Cannot read file due to syntax error:
    "    - No terminal matches '}' in the current parser context, at line 1 col 36
    let l:filename_re = '^\([^:]*\):'
    let l:linenum_re = '\([0-9]*\):'
    let l:colnum_re = '\([0-9]*\):'
    let l:err_id = '\([IWEFU][0-9]\+\):'
    let l:err_msg = '\(.*\)'
    let l:pattern =  filename_re .
    \ linenum_re .
    \ colnum_re .
    \ ' ' .
    \ err_id .
    \ ' ' .
    \ err_msg

    let l:result = []
    let l:idx = 0

    for l:line in a:lines
        let l:matches = matchlist(line, pattern)

        if len(matches) > 0
            " option: Info, Warning, Error, Fatal, Unknown
            if index(['I', 'W'], matches[4][0]) >= 0
                let l:type = 'W'
            else
                let l:type = 'E'
            endif

            let l:lnum = matches[2]
            let l:col = matches[3]
            let l:text = matches[5]

            " Better locations for some syntax errors
            if matches[4][0] is# 'F'
                let l:syntax_error_re = '^\(.*\), at line \([0-9]\+\) col \([0-9]\+\)$'
                let l:next_line = a:lines[idx+1]
                let l:syn_err_matches = matchlist(l:next_line, l:syntax_error_re)

                if len(syn_err_matches) > 0
                    let l:text = l:text . l:syn_err_matches[1]
                    let l:lnum = l:syn_err_matches[2]
                    let l:col = l:syn_err_matches[3]
                else
                    let l:text = l:next_line
                endif
            endif

            let l:element = {
            \ 'lnum': str2nr(l:lnum),
            \ 'col': str2nr(l:col),
            \ 'text': l:text,
            \ 'detail': l:matches[4] . ': ' . l:text,
            \ 'filename': fnamemodify(matches[1], ':p'),
            \ 'type': l:type
            \ }

            call add(l:result, l:element)
        endif

        let l:idx += 1
    endfor

    return result

endfun
