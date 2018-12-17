" Author: w0rp <devw0rp@gmail.com>
" Description: Parsing and transforming of LSP server responses.

" Constants for error codes.
" Defined by JSON RPC
let s:PARSE_ERROR = -32700
let s:INVALID_REQUEST = -32600
let s:METHOD_NOT_FOUND = -32601
let s:INVALID_PARAMS = -32602
let s:INTERNAL_ERROR = -32603
let s:SERVER_ERROR_START = -32099
let s:SERVER_ERROR_END = -32000
let s:SERVER_NOT_INITIALIZED = -32002
let s:UNKNOWN_ERROR_CODE = -32001
" Defined by the protocol.
let s:REQUEST_CANCELLED = -32800

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
            if type(l:diagnostic.code) == v:t_string
                let l:loclist_item.code = l:diagnostic.code
            elseif type(l:diagnostic.code) == v:t_number && l:diagnostic.code != -1
                let l:loclist_item.code = string(l:diagnostic.code)
                let l:loclist_item.nr = l:diagnostic.code
            endif
        endif

        if has_key(l:diagnostic, 'relatedInformation')
            let l:related = deepcopy(l:diagnostic.relatedInformation)
            call map(l:related, {key, val ->
                \ ale#path#FromURI(val.location.uri) .
                \ ':' . (val.location.range.start.line + 1) .
                \ ':' . (val.location.range.start.character + 1) .
                \ ":\n\t" . val.message
                \ })
            let l:loclist_item.detail = l:diagnostic.message . "\n" . join(l:related, "\n")
        endif

        if has_key(l:diagnostic, 'source')
           let l:loclist_item.detail = printf('[%s] %s', l:diagnostic.source, l:diagnostic.message)
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
            if type(l:diagnostic.code) == v:t_string
                let l:loclist_item.code = l:diagnostic.code
            elseif type(l:diagnostic.code) == v:t_number && l:diagnostic.code != -1
                let l:loclist_item.code = string(l:diagnostic.code)
                let l:loclist_item.nr = l:diagnostic.code
            endif
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

function! ale#lsp#response#GetErrorMessage(response) abort
    if type(get(a:response, 'error', 0)) isnot v:t_dict
        return ''
    endif

    let l:code = get(a:response.error, 'code')

    " Only report things for these error codes.
    if l:code isnot s:INVALID_PARAMS && l:code isnot s:INTERNAL_ERROR
        return ''
    endif

    let l:message = get(a:response.error, 'message', '')

    if empty(l:message)
        return ''
    endif

    " Include the traceback or error data as details, if present.
    let l:error_data = get(a:response.error, 'data', {})

    if type(l:error_data) is v:t_string
        let l:message .= "\n" . l:error_data
    elseif type(l:error_data) is v:t_dict
        let l:traceback = get(l:error_data, 'traceback', [])

        if type(l:traceback) is v:t_list && !empty(l:traceback)
            let l:message .= "\n" . join(l:traceback, "\n")
        endif
    endif

    return l:message
endfunction
