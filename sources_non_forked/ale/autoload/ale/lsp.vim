" Author: w0rp <devw0rp@gmail.com>
" Description: Language Server Protocol client code

" A List of connections, used for tracking servers which have been connected
" to, and programs which are run.
let s:connections = get(s:, 'connections', [])
let g:ale_lsp_next_message_id = 1

" Exposed only so tests can get at it.
" Do not call this function basically anywhere.
function! ale#lsp#NewConnection(initialization_options) abort
    " id: The job ID as a Number, or the server address as a string.
    " data: The message data received so far.
    " executable: An executable only set for program connections.
    " open_documents: A Dictionary mapping buffers to b:changedtick, keeping
    "   track of when documents were opened, and when we last changed them.
    " callback_list: A list of callbacks for handling LSP responses.
    " initialization_options: Options to send to the server.
    " capabilities: Features the server supports.
    let l:conn = {
    \   'is_tsserver': 0,
    \   'id': '',
    \   'data': '',
    \   'projects': {},
    \   'open_documents': {},
    \   'callback_list': [],
    \   'initialization_options': a:initialization_options,
    \   'capabilities': {
    \       'hover': 0,
    \       'references': 0,
    \       'completion': 0,
    \       'completion_trigger_characters': [],
    \       'definition': 0,
    \   },
    \}

    call add(s:connections, l:conn)

    return l:conn
endfunction

" Remove an LSP connection with a given ID. This is only for tests.
function! ale#lsp#RemoveConnectionWithID(id) abort
    call filter(s:connections, 'v:val.id isnot a:id')
endfunction

function! s:FindConnection(key, value) abort
    for l:conn in s:connections
        if has_key(l:conn, a:key) && get(l:conn, a:key) is# a:value
            return l:conn
        endif
    endfor

    return {}
endfunction

" Get the capabilities for a connection, or an empty Dictionary.
function! ale#lsp#GetConnectionCapabilities(id) abort
    return get(s:FindConnection('id', a:id), 'capabilities', {})
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

    " Call capabilities callbacks queued for the project.
    for [l:capability, l:Callback] in a:project.capabilities_queue
        if a:conn.is_tsserver || a:conn.capabilities[l:capability]
            call call(l:Callback, [a:conn.id, a:project.root])
        endif
    endfor

    " Clear the queued callbacks now.
    let a:project.capabilities_queue = []
endfunction

function! s:HandleInitializeResponse(conn, response) abort
    let l:request_id = a:response.request_id
    let l:project = s:FindProjectWithInitRequestID(a:conn, l:request_id)

    if !empty(l:project)
        call s:MarkProjectAsInitialized(a:conn, l:project)
    endif
endfunction

" Update capabilities from the server, so we know which features the server
" supports.
function! s:UpdateCapabilities(conn, capabilities) abort
    if type(a:capabilities) isnot v:t_dict
        return
    endif

    if get(a:capabilities, 'hoverProvider') is v:true
        let a:conn.capabilities.hover = 1
    endif

    if get(a:capabilities, 'referencesProvider') is v:true
        let a:conn.capabilities.references = 1
    endif

    if !empty(get(a:capabilities, 'completionProvider'))
        let a:conn.capabilities.completion = 1
    endif

    if type(get(a:capabilities, 'completionProvider')) is v:t_dict
        let l:chars = get(a:capabilities.completionProvider, 'triggerCharacters')

        if type(l:chars) is v:t_list
            let a:conn.capabilities.completion_trigger_characters = l:chars
        endif
    endif

    if get(a:capabilities, 'definitionProvider') is v:true
        let a:conn.capabilities.definition = 1
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
            call s:UpdateCapabilities(a:conn, a:response.result.capabilities)

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
    if type(a:message) isnot v:t_string
        " Ignore messages that aren't strings.
        return
    endif

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

function! s:HandleChannelMessage(channel_id, message) abort
    let l:address = ale#socket#GetAddress(a:channel_id)
    let l:conn = s:FindConnection('id', l:address)

    call ale#lsp#HandleMessage(l:conn, a:message)
endfunction

function! s:HandleCommandMessage(job_id, message) abort
    let l:conn = s:FindConnection('id', a:job_id)

    call ale#lsp#HandleMessage(l:conn, a:message)
endfunction

" Given a connection ID, mark it as a tsserver connection, so it will be
" handled that way.
function! ale#lsp#MarkConnectionAsTsserver(conn_id) abort
    let l:conn = s:FindConnection('id', a:conn_id)

    if !empty(l:conn)
        let l:conn.is_tsserver = 1
    endif
endfunction

" Register a project for an LSP connection.
"
" This function will throw if the connection doesn't exist.
function! ale#lsp#RegisterProject(conn_id, project_root) abort
    let l:conn = s:FindConnection('id', a:conn_id)

    " Empty strings can't be used for Dictionary keys in NeoVim, due to E713.
    " This appears to be a nonsensical bug in NeoVim.
    let l:key = empty(a:project_root) ? '<<EMPTY>>' : a:project_root

    if !has_key(l:conn.projects, l:key)
        " Tools without project roots are ready right away, like tsserver.
        let l:conn.projects[l:key] = {
        \   'root': a:project_root,
        \   'initialized': empty(a:project_root),
        \   'init_request_id': 0,
        \   'message_queue': [],
        \   'capabilities_queue': [],
        \}
    endif
endfunction

function! ale#lsp#GetProject(conn, project_root) abort
    if empty(a:conn)
        return {}
    endif

    let l:key = empty(a:project_root) ? '<<EMPTY>>' : a:project_root

    return get(a:conn.projects, l:key, {})
endfunction

" Start a program for LSP servers which run with executables.
"
" The job ID will be returned for for the program if it ran, otherwise
" 0 will be returned.
function! ale#lsp#StartProgram(executable, command, init_options) abort
    if !executable(a:executable)
        return 0
    endif

    let l:conn = s:FindConnection('executable', a:executable)

    " Get the current connection or a new one.
    let l:conn = !empty(l:conn) ? l:conn : ale#lsp#NewConnection(a:init_options)
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

    return l:job_id
endfunction

" Connect to an address and set up a callback for handling responses.
function! ale#lsp#ConnectToAddress(address, init_options) abort
    let l:conn = s:FindConnection('id', a:address)
    " Get the current connection or a new one.
    let l:conn = !empty(l:conn) ? l:conn : ale#lsp#NewConnection(a:init_options)

    if !has_key(l:conn, 'channel_id') || !ale#socket#IsOpen(l:conn.channel_id)
        let l:conn.channel_id = ale#socket#Open(a:address, {
        \   'callback': function('s:HandleChannelMessage'),
        \})
    endif

    if l:conn.channel_id < 0
        return ''
    endif

    let l:conn.id = a:address

    return a:address
endfunction

" Given a connection ID and a callback, register that callback for handling
" messages if the connection exists.
function! ale#lsp#RegisterCallback(conn_id, callback) abort
    let l:conn = s:FindConnection('id', a:conn_id)

    if !empty(l:conn)
        " Add the callback to the List if it's not there already.
        call uniq(sort(add(l:conn.callback_list, a:callback)))
    endif
endfunction

" Stop all LSP connections, closing all jobs and channels, and removing any
" queued messages.
function! ale#lsp#StopAll() abort
    for l:conn in s:connections
        if has_key(l:conn, 'channel_id')
            call ale#socket#Close(l:conn.channel_id)
        else
            call ale#job#Stop(l:conn.id)
        endif
    endfor

    let s:connections = []
endfunction

function! s:SendMessageData(conn, data) abort
    if has_key(a:conn, 'executable')
        call ale#job#SendRaw(a:conn.id, a:data)
    elseif has_key(a:conn, 'channel_id') && ale#socket#IsOpen(a:conn.channel_id)
        " Send the message to the server
        call ale#socket#Send(a:conn.channel_id, a:data)
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
    let l:project = ale#lsp#GetProject(l:conn, l:project_root)

    if empty(l:project)
        return 0
    endif

    " If we haven't initialized the server yet, then send the message for it.
    if !l:project.initialized
        " Only send the init message once.
        if !l:project.init_request_id
            let [l:init_id, l:init_data] = ale#lsp#CreateMessageData(
            \   ale#lsp#message#Initialize(l:project_root, l:conn.initialization_options),
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

" Notify LSP servers or tsserver if a document is opened, if needed.
" If a document is opened, 1 will be returned, otherwise 0 will be returned.
function! ale#lsp#OpenDocument(conn_id, project_root, buffer, language_id) abort
    let l:conn = s:FindConnection('id', a:conn_id)
    let l:opened = 0

    " FIXME: Return 1 if the document is already open?
    if !empty(l:conn) && !has_key(l:conn.open_documents, a:buffer)
        if l:conn.is_tsserver
            let l:message = ale#lsp#tsserver_message#Open(a:buffer)
        else
            let l:message = ale#lsp#message#DidOpen(a:buffer, a:language_id)
        endif

        call ale#lsp#Send(a:conn_id, l:message, a:project_root)
        let l:conn.open_documents[a:buffer] = getbufvar(a:buffer, 'changedtick')
        let l:opened = 1
    endif

    return l:opened
endfunction

" Notify LSP servers or tsserver that a document has changed, if needed.
" If a notification is sent, 1 will be returned, otherwise 0 will be returned.
function! ale#lsp#NotifyForChanges(conn_id, project_root, buffer) abort
    let l:conn = s:FindConnection('id', a:conn_id)
    let l:notified = 0

    if !empty(l:conn) && has_key(l:conn.open_documents, a:buffer)
        let l:new_tick = getbufvar(a:buffer, 'changedtick')

        if l:conn.open_documents[a:buffer] < l:new_tick
            if l:conn.is_tsserver
                let l:message = ale#lsp#tsserver_message#Change(a:buffer)
            else
                let l:message = ale#lsp#message#DidChange(a:buffer)
            endif

            call ale#lsp#Send(a:conn_id, l:message, a:project_root)
            let l:conn.open_documents[a:buffer] = l:new_tick
            let l:notified = 1
        endif
    endif

    return l:notified
endfunction

" Given some LSP details that must contain at least `connection_id` and
" `project_root` keys,
function! ale#lsp#WaitForCapability(conn_id, project_root, capability, callback) abort
    let l:conn = s:FindConnection('id', a:conn_id)
    let l:project = ale#lsp#GetProject(l:conn, a:project_root)

    if empty(l:project)
        return 0
    endif

    if type(get(l:conn.capabilities, a:capability, v:null)) isnot v:t_number
        throw 'Invalid capability ' . a:capability
    endif

    if l:project.initialized
        if l:conn.is_tsserver || l:conn.capabilities[a:capability]
            " The project has been initialized, so call the callback now.
            call call(a:callback, [a:conn_id, a:project_root])
        endif
    else
        " Call the callback later, once we have the information we need.
        call add(l:project.capabilities_queue, [a:capability, a:callback])
    endif
endfunction
