" Author: Daniel Schemala <istjanichtzufassen@gmail.com>,
"   w0rp <devw0rp@gmail.com>
"
" Description: This file implements handlers specific to Rust.

if !exists('g:ale_rust_ignore_error_codes')
    let g:ale_rust_ignore_error_codes = []
endif

function! s:FindSpan(buffer, span) abort
    if ale#path#IsBufferPath(a:buffer, a:span.file_name) || a:span.file_name is# '<anon>'
        return a:span
    endif

    " Search inside the expansion of an error, as the problem for this buffer
    " could lie inside a nested object.
    if !empty(get(a:span, 'expansion', v:null))
        return s:FindSpan(a:buffer, a:span.expansion.span)
    endif

    return {}
endfunction

function! ale#handlers#rust#HandleRustErrors(buffer, lines) abort
    let l:output = []

    for l:errorline in a:lines
        " ignore everything that is not JSON
        if l:errorline !~# '^{'
            continue
        endif

        let l:error = json_decode(l:errorline)

        if has_key(l:error, 'message') && type(l:error.message) is v:t_dict
            let l:error = l:error.message
        endif

        if !has_key(l:error, 'code')
            continue
        endif

        if !empty(l:error.code) && index(g:ale_rust_ignore_error_codes, l:error.code.code) > -1
            continue
        endif

        for l:root_span in l:error.spans
            let l:span = s:FindSpan(a:buffer, l:root_span)

            if !empty(l:span)
                call add(l:output, {
                \   'lnum': l:span.line_start,
                \   'end_lnum': l:span.line_end,
                \   'col': l:span.column_start,
                \   'end_col': l:span.column_end,
                \   'text': empty(l:span.label) ? l:error.message : printf('%s: %s', l:error.message, l:span.label),
                \   'type': toupper(l:error.level[0]),
                \})
            endif
        endfor
    endfor

    return l:output
endfunction
