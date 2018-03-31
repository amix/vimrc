" Author: w0rp <devw0rp@gmail.com>
" Description: Language Server Protocol client code

" A List of connections, used for tracking servers which have been connected
" to, and programs which are run.
let s:connections = []
let g:ale_lsp_next_message_id = 1

function! s:NewConnection() abort
    " id: The job ID as a Number, or the server address as a string.
    " data: The message data received so far.
    " executable: An executable only set for program connections.
    " open_documents: A list of buffers we told the server we opened.
    " callback_list: A list of callbacks for handling LSP responses.
    let l:conn = {
    \   'id': '',
    \   'data': '',
    \   'projects': {},
    \   'open_documents': [],
    \   'callback_list': [],
    \}

    call add(s:connections, l:conn)

    return l:conn
endfunction

function! s:FindConnection(key, value) abort
    for l:conn in s:connections
        if has_key(l:conn, a:key) && get(l:conn, a:key) == a:value
            return l:conn
        endif
    endfor

    return {}
endfunction

function! ale#lsp#GetNextMessageID() abort
    " Use the current ID
    let l:id = g:ale_lsp_next_message_id

    " Increment the ID variable.
    let g:ale_lsp_next_message_id += 1

    " When the ID overflows, reset it to 1. By the time we hit the initial ID
    " again, the messages will be long gone.
    if g:ale_lsp_next_message_id < 1
        let g:ale_lsp_next_message_id = 1
    endif

    return l:id
endfunction

" TypeScript messages use a different format.
function! s:CreateTSServerMessageData(message) abort
    let l:is_notification = a:message[0]

    let l:obj = {
    \   'seq': v:null,
    \   'type': 'request',
    \   'command': a:message[1][3:],
    \}

    if !l:is_notification
        let l:obj.seq = ale#lsp#GetNextMessageID()
    endif

    if len(a:message) > 2
        let l:obj.arguments = a:message[2]
    endif

    let l:data = json_encode(l:obj) . "\n"
    return [l:is_notification ? 0 : l:obj.seq, l:data]
endfunction

" Given a List of one or two items, [method_name] or [method_name, params],
" return a List containing [message_id, message_data]
function! ale#lsp#CreateMessageData(message) abort
    if a:message[1] =~# '^ts@'
        return s:CreateTSServerMessageData(a:message)
    endif

    let l:is_notification = a:message[0]

    let l:obj = {
    \   'method': a:message[1],
    \   'jsonrpc': '2.0',
    \}

    if !l:is_notification
        let l:obj.id = ale#lsp#GetNextMessageID()
    endif

    if len(a:message) > 2
        let l:obj.params = a:message[2]
    endif

    let l:body = json_encode(l:obj)
    let l:data = 'Content-Length: ' . strlen(l:body) . "\r\n\r\n" . l:body

    return [l:is_notification ? 0 : l:obj.id, l:data]
endfunction

function! ale#lsp#ReadMessageData(data) abort
    let l:response_list = []
    let l:remainder = a:data

    while 1
        " Look for the end of the HTTP headers
        let l:body_start_index = matchend(l:remainder, "\r\n\r\n")

        if l:body_start_index < 0
            " No header end was found yet.
            break
        endif

        " Parse the Content-Length header.
        let l:header_data = l:remainder[:l:body_start_index - 4]
        let l:length_match = matchlist(
        \   l:header_data,
        \   '\vContent-Length: *(\d+)'
        \)

        if empty(l:length_match)
            throw "Invalid JSON-RPC header:\n" . l:header_data
        endif

        " Split the body and the remainder of the text.
        let l:remainder_start_index = l:body_start_index + str2nr(l:length_match[1])

        if len(l:remainder) < l:remainder_start_index
            " We don't have enough data yet.
            break
        endif

        let l:body = l:remainder[l:body_start_index : l:remainder_start_index - 1]
        let l:remainder = l:remainder[l:remainder_start_index :]

        " Parse the JSON object and add it to the list.
        call add(l:response_list, json_decode(l:body))
    endwhile

    return [l:remainder, l:response_list]
endfunction

function! s:FindProjectWithInitRequestID(conn, init_request_id) abort
    for l:project_root in keys(a:conn.projects)
        let l:project = a:conn.projects[l:project_root]

        if l:project.init_request_id == a:init_request_id
            return l:project
        endif
    endfor

    return {}
endfunction

function! s:MarkProjectAsInitialized(conn, project) abort
    let a:project.initialized = 1

    " After the server starts, send messages we had queued previously.
    for l:message_data in a:project.message_queue
        call s:SendMessageData(a:conn, l:message_data)
    endfor

    " Remove the messages now.
    let a:conn.message_queue = []
endfunction

function! s:HandleInitializeResponse(conn, response) abort
    let l:request_id = a:response.request_id
    let l:project = s:FindProjectWithInitRequestID(a:conn, l:request_id)

    if !empty(l:project)
        call s:MarkProjectAsInitialized(a:conn, l:project)
    endif
endfunction

function! ale#lsp#HandleOtherInitializeResponses(conn, response) abort
    let l:uninitialized_projects = []

    for [l:key, l:value] in items(a:conn.projects)
        if l:value.initialized == 0
            call add(l:uninitialized_projects, [l:key, l:value])
        endif
    endfor

    if empty(l:uninitialized_projects)
        return
    endif

    if get(a:response, 'method', '') is# ''
        if has_key(get(a:response, 'result', {}), 'capabilities')
            for [l:dir, l:project] in l:uninitialized_projects
                call s:MarkProjectAsInitialized(a:conn, l:project)
            endfor
        endif
    elseif get(a:response, 'method', '') is# 'textDocument/publishDiagnostics'
        let l:filename = ale#path#FromURI(a:response.params.uri)

        for [l:dir, l:project] in l:uninitialized_projects
            if l:filename[:len(l:dir) - 1] is# l:dir
                call s:MarkProjectAsInitialized(a:conn, l:project)
            endif
        endfor
    endif
endfunction

function! ale#lsp#HandleMessage(conn, message) abort
    let a:conn.data .= a:message

    " Parse the objects now if we can, and keep the remaining text.
    let [a:conn.data, l:response_list] = ale#lsp#ReadMessageData(a:conn.data)

    " Call our callbacks.
    for l:response in l:response_list
        if get(l:response, 'method', '') is# 'initialize'
            call s:HandleInitializeResponse(a:conn, l:response)
        else
            call ale#lsp#HandleOtherInitializeResponses(a:conn, l:response)

            " Call all of the registered handlers with the response.
            for l:Callback in a:conn.callback_list
                call ale#util#GetFunction(l:Callback)(a:conn.id, l:response)
            endfor
        endif
    endfor
endfunction

function! s:HandleChannelMessage(channel, message) abort
    let l:info = ch_info(a:channel)
    let l:address = l:info.hostname . l:info.address
    let l:conn = s:FindConnection('id', l:address)

    call ale#lsp#HandleMessage(l:conn, a:message)
endfunction

function! s:HandleCommandMessage(job_id, message) abort
    let l:conn = s:FindConnection('id', a:job_id)

    call ale#lsp#HandleMessage(l:conn, a:message)
endfunction

function! ale#lsp#RegisterProject(conn, project_root) abort
    " Empty strings can't be used for Dictionary keys in NeoVim, due to E713.
    " This appears to be a nonsensical bug in NeoVim.
    let l:key = empty(a:project_root) ? '<<EMPTY>>' : a:project_root

    if !has_key(a:conn.projects, l:key)
        " Tools without project roots are ready right away, like tsserver.
        let a:conn.projects[l:key] = {
        \   'initialized': empty(a:project_root),
        \   'init_request_id': 0,
        \   'message_queue': [],
        \}
    endif
endfunction

function! ale#lsp#GetProject(conn, project_root) abort
    let l:key = empty(a:project_root) ? '<<EMPTY>>' : a:project_root

    return get(a:conn.projects, l:key, {})
endfunction

" Start a program for LSP servers which run with executables.
"
" The job ID will be returned for for the program if it ran, otherwise
" 0 will be returned.
function! ale#lsp#StartProgram(executable, command, project_root, callback) abort
    if !executable(a:executable)
        return 0
    endif

    let l:conn = s:FindConnection('executable', a:executable)

    " Get the current connection or a new one.
    let l:conn = !empty(l:conn) ? l:conn : s:NewConnection()
    let l:conn.executable = a:executable

    if !has_key(l:conn, 'id') || !ale#job#IsRunning(l:conn.id)
        let l:options = {
        \   'mode': 'raw',
        \   'out_cb': function('s:HandleCommandMessage'),
        \}
        let l:job_id = ale#job#Start(a:command, l:options)
    else
        let l:job_id = l:conn.id
    endif

    if l:job_id <= 0
        return 0
    endif

    let l:conn.id = l:job_id
    " Add the callback to the List if it's not there already.
    call uniq(sort(add(l:conn.callback_list, a:callback)))
    call ale#lsp#RegisterProject(l:conn, a:project_root)

    return l:job_id
endfunction

" Connect to an address and set up a callback for handling responses.
function! ale#lsp#ConnectToAddress(address, project_root, callback) abort
    let l:conn = s:FindConnection('id', a:address)
    " Get the current connection or a new one.
    let l:conn = !empty(l:conn) ? l:conn : s:NewConnection()

    if !has_key(l:conn, 'channel') || ch_status(l:conn.channel) isnot# 'open'
        let l:conn.channnel = ch_open(a:address, {
        \   'mode': 'raw',
        \   'waittime': 0,
        \   'callback': function('s:HandleChannelMessage'),
        \})
    endif

    if ch_status(l:conn.channnel) is# 'fail'
        return 0
    endif

    let l:conn.id = a:address
    " Add the callback to the List if it's not there already.
    call uniq(sort(add(l:conn.callback_list, a:callback)))
    call ale#lsp#RegisterProject(l:conn, a:project_root)

    return 1
endfunction

" Stop all LSP connections, closing all jobs and channels, and removing any
" queued messages.
function! ale#lsp#StopAll() abort
    for l:conn in s:connections
        if has_key(l:conn, 'channel')
            call ch_close(l:conn.channel)
        else
            call ale#job#Stop(l:conn.id)
        endif
    endfor

    let s:connections = []
endfunction

function! s:SendMessageData(conn, data) abort
    if has_key(a:conn, 'executable')
        call ale#job#SendRaw(a:conn.id, a:data)
    elseif has_key(a:conn, 'channel') && ch_status(a:conn.channnel) is# 'open'
        " Send the message to the server
        call ch_sendraw(a:conn.channel, a:data)
    else
        return 0
    endif

    return 1
endfunction

" Send a message to an LSP server.
" Notifications do not need to be handled.
"
" Returns -1 when a message is sent, but no response is expected
"          0 when the message is not sent and
"          >= 1 with the message ID when a response is expected.
function! ale#lsp#Send(conn_id, message, ...) abort
    let l:project_root = get(a:000, 0, '')

    let l:conn = s:FindConnection('id', a:conn_id)

    if empty(l:conn)
        return 0
    endif

    let l:project = ale#lsp#GetProject(l:conn, l:project_root)

    if empty(l:project)
        return 0
    endif

    " If we haven't initialized the server yet, then send the message for it.
    if !l:project.initialized
        " Only send the init message once.
        if !l:project.init_request_id
            let [l:init_id, l:init_data] = ale#lsp#CreateMessageData(
            \   ale#lsp#message#Initialize(l:project_root),
            \)

            let l:project.init_request_id = l:init_id

            call s:SendMessageData(l:conn, l:init_data)
        endif
    endif

    let [l:id, l:data] = ale#lsp#CreateMessageData(a:message)

    if l:project.initialized
        " Send the message now.
        call s:SendMessageData(l:conn, l:data)
    else
        " Add the message we wanted to send to a List to send later.
        call add(l:project.message_queue, l:data)
    endif

    return l:id == 0 ? -1 : l:id
endfunction

function! ale#lsp#OpenDocumentIfNeeded(conn_id, buffer, project_root, language_id) abort
    let l:conn = s:FindConnection('id', a:conn_id)
    let l:opened = 0

    if !empty(l:conn) && index(l:conn.open_documents, a:buffer) < 0
        if empty(a:language_id)
            let l:message = ale#lsp#tsserver_message#Open(a:buffer)
        else
            let l:message = ale#lsp#message#DidOpen(a:buffer, a:language_id)
        endif

        call ale#lsp#Send(a:conn_id, l:message, a:project_root)
        call add(l:conn.open_documents, a:buffer)
        let l:opened = 1
    endif

    return l:opened
endfunction
