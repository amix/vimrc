" Author: w0rp <devw0rp@gmail.com>
" Description: gcc for Fortran files

" This option can be set to 0 to use -ffixed-form
call ale#Set('fortran_gcc_use_free_form', 1)
call ale#Set('fortran_gcc_executable', 'gcc')
" Set this option to change the GCC options for warnings for Fortran.
call ale#Set('fortran_gcc_options', '-Wall')

function! ale_linters#fortran#gcc#Handle(buffer, lines) abort
    " We have to match a starting line and a later ending line together,
    " like so.
    "
    " :21.34:
    " Error: Expected comma in I/O list at (1)
    let l:line_marker_pattern = ':\(\d\+\)[.:]\=\(\d\+\)\=:\=$'
    let l:message_pattern = '^\(Error\|Warning\): \(.\+\)$'
    let l:looking_for_message = 0
    let l:last_loclist_obj = {}

    let l:output = []

    for l:line in a:lines
        if l:looking_for_message
            let l:match = matchlist(l:line, l:message_pattern)
        else
            let l:match = matchlist(l:line, l:line_marker_pattern)
        endif

        if len(l:match) == 0
            continue
        endif

        if l:looking_for_message
            let l:looking_for_message = 0

            " Now we have the text, we can set it and add the error.
            let l:last_loclist_obj.text = l:match[2]
            let l:last_loclist_obj.type = l:match[1] is# 'Warning' ? 'W' : 'E'
            call add(l:output, l:last_loclist_obj)
        else
            let l:last_loclist_obj = {
            \   'bufnr': a:buffer,
            \   'lnum': l:match[1] + 0,
            \   'col': l:match[2] + 0,
            \}

            " Start looking for the message and error type.
            let l:looking_for_message = 1
        endif
    endfor

    return l:output
endfunction

function! ale_linters#fortran#gcc#GetCommand(buffer) abort
    let l:layout_option = ale#Var(a:buffer, 'fortran_gcc_use_free_form')
    \   ? '-ffree-form'
    \   : '-ffixed-form'

    return '%e -S -x f95 -fsyntax-only ' . l:layout_option
    \   . ale#Pad(ale#Var(a:buffer, 'fortran_gcc_options'))
    \   . ' -'
endfunction

call ale#linter#Define('fortran', {
\   'name': 'gcc',
\   'output_stream': 'stderr',
\   'executable_callback': ale#VarFunc('fortran_gcc_executable'),
\   'command_callback': 'ale_linters#fortran#gcc#GetCommand',
\   'callback': 'ale_linters#fortran#gcc#Handle',
\})
