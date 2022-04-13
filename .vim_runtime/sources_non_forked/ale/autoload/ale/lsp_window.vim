" Author: suoto <andre820@gmail.com>
" Description: Handling of window/* LSP methods, although right now only
" handles window/showMessage

" Constants for message type codes
let s:LSP_MESSAGE_TYPE_DISABLED = 0
let s:LSP_MESSAGE_TYPE_ERROR = 1
let s:LSP_MESSAGE_TYPE_WARNING = 2
let s:LSP_MESSAGE_TYPE_INFORMATION = 3
let s:LSP_MESSAGE_TYPE_LOG = 4

" Translate strings from the user config to a number so we can check
" severities
let s:CFG_TO_LSP_SEVERITY = {
\   'disabled': s:LSP_MESSAGE_TYPE_DISABLED,
\   'error': s:LSP_MESSAGE_TYPE_ERROR,
\   'warning': s:LSP_MESSAGE_TYPE_WARNING,
\   'information': s:LSP_MESSAGE_TYPE_INFORMATION,
\   'info': s:LSP_MESSAGE_TYPE_INFORMATION,
\   'log': s:LSP_MESSAGE_TYPE_LOG
\}

" Handle window/showMessage response.
" - details: dict containing linter name and format (g:ale_lsp_show_message_format)
" - params: dict with the params for the call in the form of {type: number, message: string}
function! ale#lsp_window#HandleShowMessage(linter_name, format, params) abort
    let l:message = a:params.message
    let l:type = a:params.type

    " Get the configured severity level threshold and check if the message
    " should be displayed or not
    let l:configured_severity = tolower(get(g:, 'ale_lsp_show_message_severity', 'error'))
    " If the user has configured with a value we can't find on the conversion
    " dict, fall back to warning
    let l:cfg_severity_threshold = get(s:CFG_TO_LSP_SEVERITY, l:configured_severity, s:LSP_MESSAGE_TYPE_WARNING)

    if l:type > l:cfg_severity_threshold
        return
    endif

    " Severity will depend on the message type
    if l:type is# s:LSP_MESSAGE_TYPE_ERROR
        let l:severity = g:ale_echo_msg_error_str
    elseif l:type is# s:LSP_MESSAGE_TYPE_INFORMATION
        let l:severity = g:ale_echo_msg_info_str
    elseif l:type is# s:LSP_MESSAGE_TYPE_LOG
        let l:severity = g:ale_echo_msg_log_str
    else
        " Default to warning just in case
        let l:severity = g:ale_echo_msg_warning_str
    endif

    let l:string = substitute(a:format, '\V%severity%', l:severity, 'g')
    let l:string = substitute(l:string, '\V%linter%', a:linter_name, 'g')
    let l:string = substitute(l:string, '\V%s\>', l:message, 'g')

    call ale#util#ShowMessage(l:string)
endfunction
