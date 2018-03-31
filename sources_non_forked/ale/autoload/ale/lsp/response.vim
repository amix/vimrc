" Author: w0rp <devw0rp@gmail.com>
" Description: Parsing and transforming of LSP server responses.

" Constants for message severity codes.
let s:SEVERITY_ERROR = 1
let s:SEVERITY_WARNING = 2
let s:SEVERITY_INFORMATION = 3
let s:SEVERITY_HINT = 4

" Parse the message for textDocument/publishDiagnostics
function! ale#lsp#response#ReadDiagnostics(response) abort
    let l:loclist = []

    for l:diagnostic in a:response.params.diagnostics
        let l:severity = get(l:diagnostic, 'severity', 0)
        let l:loclist_item = {
        \   'text': l:diagnostic.message,
        \   'type': 'E',
        \   'lnum': l:diagnostic.range.start.line + 1,
        \   'col': l:diagnostic.range.start.character + 1,
        \   'end_lnum': l:diagnostic.range.end.line + 1,
        \   'end_col': l:diagnostic.range.end.character + 1,
        \}

        if l:severity == s:SEVERITY_WARNING
            let l:loclist_item.type = 'W'
        elseif l:severity == s:SEVERITY_INFORMATION
            " TODO: Use 'I' here in future.
            let l:loclist_item.type = 'W'
        elseif l:severity == s:SEVERITY_HINT
            " TODO: Use 'H' here in future
            let l:loclist_item.type = 'W'
        endif

        if has_key(l:diagnostic, 'code')
            let l:loclist_item.nr = l:diagnostic.code
        endif

        call add(l:loclist, l:loclist_item)
    endfor

    return l:loclist
endfunction

function! ale#lsp#response#ReadTSServerDiagnostics(response) abort
    let l:loclist = []

    for l:diagnostic in a:response.body.diagnostics
        let l:loclist_item = {
        \   'text': l:diagnostic.text,
        \   'type': 'E',
        \   'lnum': l:diagnostic.start.line,
        \   'col': l:diagnostic.start.offset,
        \   'end_lnum': l:diagnostic.end.line,
        \   'end_col': l:diagnostic.end.offset,
        \}

        if has_key(l:diagnostic, 'code')
            let l:loclist_item.nr = l:diagnostic.code
        endif

        if get(l:diagnostic, 'category') is# 'warning'
            let l:loclist_item.type = 'W'
        endif

        if get(l:diagnostic, 'category') is# 'suggestion'
            let l:loclist_item.type = 'I'
        endif

        call add(l:loclist, l:loclist_item)
    endfor

    return l:loclist
endfunction
