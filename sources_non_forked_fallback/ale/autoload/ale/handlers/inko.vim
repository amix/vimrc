" Author: Yorick Peterse <yorick@yorickpeterse.com>
" Description: output handlers for the Inko JSON format

function! ale#handlers#inko#GetType(severity) abort
    if a:severity is? 'warning'
        return 'W'
    endif

    return 'E'
endfunction

function! ale#handlers#inko#Handle(buffer, lines) abort
    try
        let l:errors = json_decode(join(a:lines, ''))
    catch
        return []
    endtry

    if empty(l:errors)
        return []
    endif

    let l:output = []
    let l:dir = expand('#' . a:buffer . ':p:h')

    for l:error in l:errors
        call add(l:output, {
        \   'filename': ale#path#GetAbsPath(l:dir, l:error['file']),
        \   'lnum': l:error['line'],
        \   'col': l:error['column'],
        \   'text': l:error['message'],
        \   'type': ale#handlers#inko#GetType(l:error['level']),
        \})
    endfor

    return l:output
endfunction
