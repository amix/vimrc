" Author: Johannes Wienke <languitar@semipol.de>
" Description: output handler for the vale JSON format

function! ale#handlers#vale#GetType(severity) abort
    if a:severity is? 'warning'
        return 'W'
    elseif a:severity is? 'suggestion'
        return 'I'
    endif

    return 'E'
endfunction

function! ale#handlers#vale#Handle(buffer, lines) abort
    try
        let l:errors = json_decode(join(a:lines, ''))
    catch
        return []
    endtry

    if empty(l:errors)
        return []
    endif

    let l:output = []

    for l:error in l:errors[keys(l:errors)[0]]
        call add(l:output, {
        \   'lnum': l:error['Line'],
        \   'col': l:error['Span'][0],
        \   'end_col': l:error['Span'][1],
        \   'code': l:error['Check'],
        \   'text': l:error['Message'],
        \   'type': ale#handlers#vale#GetType(l:error['Severity']),
        \})
    endfor

    return l:output
endfunction
