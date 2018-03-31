" Author: w0rp <devw0rp@gmail.com>
" Description: Generic fixer functions for Python.

" Add blank lines before control statements.
function! ale#fixers#generic_python#AddLinesBeforeControlStatements(buffer, lines) abort
    let l:new_lines = []
    let l:last_indent_size = 0
    let l:last_line_is_blank = 0

    for l:line in a:lines
        let l:indent_size = len(matchstr(l:line, '^ *'))

        if !l:last_line_is_blank
        \&& l:indent_size <= l:last_indent_size
        \&& match(l:line, '\v^ *(return|if|for|while|break|continue)') >= 0
            call add(l:new_lines, '')
        endif

        call add(l:new_lines, l:line)
        let l:last_indent_size = l:indent_size
        let l:last_line_is_blank = empty(split(l:line))
    endfor

    return l:new_lines
endfunction

" This function breaks up long lines so that autopep8 or other tools can
" fix the badly-indented code which is produced as a result.
function! ale#fixers#generic_python#BreakUpLongLines(buffer, lines) abort
    " Default to a maximum line length of 79
    let l:max_line_length = 79
    let l:conf = ale#path#FindNearestFile(a:buffer, 'setup.cfg')

    " Read the maximum line length from setup.cfg
    if !empty(l:conf)
        for l:match in ale#util#GetMatches(
        \   readfile(l:conf),
        \   '\v^ *max-line-length *\= *(\d+)',
        \)
            let l:max_line_length = str2nr(l:match[1])
        endfor
    endif

    let l:new_list = []

    for l:line in a:lines
        if len(l:line) > l:max_line_length && l:line !~# '# *noqa'
            let l:line = substitute(l:line, '\v([(,])([^)])', '\1\n\2', 'g')
            let l:line = substitute(l:line, '\v([^(])([)])', '\1,\n\2', 'g')

            for l:split_line in split(l:line, "\n")
                call add(l:new_list, l:split_line)
            endfor
        else
            call add(l:new_list, l:line)
        endif
    endfor

    return l:new_list
endfunction
