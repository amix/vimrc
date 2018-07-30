" Author: w0rp <devw0rp@gmail.com>
" Description: Integration between linters and LSP/tsserver.

" This code isn't loaded if a user never users LSP features or linters.

" Associates LSP connection IDs with linter names.
if !has_key(s:, 'lsp_linter_map')
    let s:lsp_linter_map = {}
endif

" Check if diagnostics for a particular linter should be ignored.
function! s:ShouldIgnore(buffer, linter_name) abort
    let l:config = ale#Var(a:buffer, 'linters_ignore')

    " Don't load code for ignoring diagnostics if there's nothing to ignore.
    if empty(l:config)
        return 0
    endif

    let l:filetype = getbufvar(a:buffer, '&filetype')
    let l:ignore_list = ale#engine#ignore#GetList(l:filetype, l:config)

    return index(l:ignore_list, a:linter_name) >= 0
endfunction

function! s:HandleLSPDiagnostics(conn_id, response) abort
    let l:linter_name = s:lsp_linter_map[a:conn_id]
    let l:filename = ale#path#FromURI(a:response.params.uri)
    let l:buffer = bufnr(l:filename)

    if s:ShouldIgnore(l:buffer, l:linter_name)
        return
    endif

    if l:buffer <= 0
        return
    endif

    let l:loclist = ale#lsp#response#ReadDiagnostics(a:response)

    call ale#engine#HandleLoclist(l:linter_name, l:buffer, l:loclist)
endfunction

function! s:HandleTSServerDiagnostics(response, error_type) abort
    let l:linter_name = 'tsserver'
    let l:buffer = bufnr(a:response.body.file)
    let l:info = get(g:ale_buffer_info, l:buffer, {})

    if empty(l:info)
        return
    endif

    if s:ShouldIgnore(l:buffer, l:linter_name)
        return
    endif

    let l:thislist = ale#lsp#response#ReadTSServerDiagnostics(a:response)

    " tsserver sends syntax and semantic errors in separate messages, so we
    " have to collect the messages separately for each buffer and join them
    " back together again.
    if a:error_type is# 'syntax'
        let l:info.syntax_loclist = l:thislist
    else
        let l:info.semantic_loclist = l:thislist
    endif

    let l:loclist = get(l:info, 'semantic_loclist', [])
    \   + get(l:info, 'syntax_loclist', [])

    call ale#engine#HandleLoclist(l:linter_name, l:buffer, l:loclist)
endfunction

function! s:HandleLSPErrorMessage(linter_name, response) abort
    if !g:ale_history_enabled || !g:ale_history_log_output
        return
    endif

    if empty(a:linter_name)
        return
    endif

    let l:message = ale#lsp#response#GetErrorMessage(a:response)

    if empty(l:message)
        return
    endif

    " This global variable is set here so we don't load the debugging.vim file
    " until someone uses :ALEInfo.
    let g:ale_lsp_error_messages = get(g:, 'ale_lsp_error_messages', {})

    if !has_key(g:ale_lsp_error_messages, a:linter_name)
        let g:ale_lsp_error_messages[a:linter_name] = []
    endif

    call add(g:ale_lsp_error_messages[a:linter_name], l:message)
endfunction

function! ale#lsp_linter#HandleLSPResponse(conn_id, response) abort
    let l:method = get(a:response, 'method', '')
    let l:linter_name = get(s:lsp_linter_map, a:conn_id, '')

    if get(a:response, 'jsonrpc', '') is# '2.0' && has_key(a:response, 'error')
        call s:HandleLSPErrorMessage(l:linter_name, a:response)
    elseif l:method is# 'textDocument/publishDiagnostics'
        call s:HandleLSPDiagnostics(a:conn_id, a:response)
    elseif get(a:response, 'type', '') is# 'event'
    \&& get(a:response, 'event', '') is# 'semanticDiag'
        call s:HandleTSServerDiagnostics(a:response, 'semantic')
    elseif get(a:response, 'type', '') is# 'event'
    \&& get(a:response, 'event', '') is# 'syntaxDiag'
        call s:HandleTSServerDiagnostics(a:response, 'syntax')
    endif
endfunction

function! ale#lsp_linter#GetOptions(buffer, linter) abort
    let l:initialization_options = {}

    if has_key(a:linter, 'initialization_options_callback')
        let l:initialization_options = ale#util#GetFunction(a:linter.initialization_options_callback)(a:buffer)
    elseif has_key(a:linter, 'initialization_options')
        let l:initialization_options = a:linter.initialization_options
    endif

    return l:initialization_options
endfunction

" Given a buffer, an LSP linter, start up an LSP linter and get ready to
" receive messages for the document.
function! ale#lsp_linter#StartLSP(buffer, linter) abort
    let l:command = ''
    let l:address = ''
    let l:root = ale#util#GetFunction(a:linter.project_root_callback)(a:buffer)

    if empty(l:root) && a:linter.lsp isnot# 'tsserver'
        " If there's no project root, then we can't check files with LSP,
        " unless we are using tsserver, which doesn't use project roots.
        return {}
    endif

    let l:init_options = ale#lsp_linter#GetOptions(a:buffer, a:linter)

    if a:linter.lsp is# 'socket'
        let l:address = ale#linter#GetAddress(a:buffer, a:linter)
        let l:conn_id = ale#lsp#ConnectToAddress(l:address, l:init_options)
    else
        let l:executable = ale#linter#GetExecutable(a:buffer, a:linter)

        if empty(l:executable) || !executable(l:executable)
            return {}
        endif

        let l:command = ale#linter#GetCommand(a:buffer, a:linter)
        " Format the command, so %e can be formatted into it.
        let l:command = ale#command#FormatCommand(a:buffer, l:executable, l:command, 0)[1]
        let l:command = ale#job#PrepareCommand(a:buffer, l:command)
        let l:conn_id = ale#lsp#StartProgram(
        \   l:executable,
        \   l:command,
        \   l:init_options,
        \)
    endif

    if empty(l:conn_id)
        if g:ale_history_enabled && !empty(l:command)
            call ale#history#Add(a:buffer, 'failed', l:conn_id, l:command)
        endif

        return {}
    endif

    " tsserver behaves differently, so tell the LSP API that it is tsserver.
    if a:linter.lsp is# 'tsserver'
        call ale#lsp#MarkConnectionAsTsserver(l:conn_id)
    endif

    " Register the project now the connection is ready.
    call ale#lsp#RegisterProject(l:conn_id, l:root)

    let l:language_id = ale#util#GetFunction(a:linter.language_callback)(a:buffer)

    let l:details = {
    \   'buffer': a:buffer,
    \   'connection_id': l:conn_id,
    \   'command': l:command,
    \   'project_root': l:root,
    \   'language_id': l:language_id,
    \}

    if ale#lsp#OpenDocument(l:conn_id, l:root, a:buffer, l:language_id)
        if g:ale_history_enabled && !empty(l:command)
            call ale#history#Add(a:buffer, 'started', l:conn_id, l:command)
        endif
    endif

    " The change message needs to be sent for tsserver before doing anything.
    if a:linter.lsp is# 'tsserver'
        call ale#lsp#NotifyForChanges(l:conn_id, l:root, a:buffer)
    endif

    return l:details
endfunction

function! ale#lsp_linter#CheckWithLSP(buffer, linter) abort
    let l:info = g:ale_buffer_info[a:buffer]
    let l:lsp_details = ale#lsp_linter#StartLSP(a:buffer, a:linter)

    if empty(l:lsp_details)
        return 0
    endif

    let l:id = l:lsp_details.connection_id
    let l:root = l:lsp_details.project_root

    " Register a callback now for handling errors now.
    let l:Callback = function('ale#lsp_linter#HandleLSPResponse')
    call ale#lsp#RegisterCallback(l:id, l:Callback)

    " Remember the linter this connection is for.
    let s:lsp_linter_map[l:id] = a:linter.name

    if a:linter.lsp is# 'tsserver'
        let l:message = ale#lsp#tsserver_message#Geterr(a:buffer)
        let l:notified = ale#lsp#Send(l:id, l:message, l:root) != 0
    else
        let l:notified = ale#lsp#NotifyForChanges(l:id, l:root, a:buffer)
    endif

    " If this was a file save event, also notify the server of that.
    if a:linter.lsp isnot# 'tsserver'
    \&& getbufvar(a:buffer, 'ale_save_event_fired', 0)
        let l:save_message = ale#lsp#message#DidSave(a:buffer)
        let l:notified = ale#lsp#Send(l:id, l:save_message, l:root) != 0
    endif

    if l:notified
        if index(l:info.active_linter_list, a:linter.name) < 0
            call add(l:info.active_linter_list, a:linter.name)
        endif
    endif

    return l:notified
endfunction

" Clear LSP linter data for the linting engine.
function! ale#lsp_linter#ClearLSPData() abort
    let s:lsp_linter_map = {}
endfunction

" Just for tests.
function! ale#lsp_linter#SetLSPLinterMap(replacement_map) abort
    let s:lsp_linter_map = a:replacement_map
endfunction
