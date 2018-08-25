let s:references_map = {}

" Used to get the references map in tests.
function! ale#references#GetMap() abort
    return deepcopy(s:references_map)
endfunction

" Used to set the references map in tests.
function! ale#references#SetMap(map) abort
    let s:references_map = a:map
endfunction

function! ale#references#ClearLSPData() abort
    let s:references_map = {}
endfunction

function! ale#references#HandleTSServerResponse(conn_id, response) abort
    if get(a:response, 'command', '') is# 'references'
    \&& has_key(s:references_map, a:response.request_seq)
        call remove(s:references_map, a:response.request_seq)

        if get(a:response, 'success', v:false) is v:true
            let l:item_list = []

            for l:response_item in a:response.body.refs
                call add(l:item_list, {
                \ 'filename': l:response_item.file,
                \ 'line': l:response_item.start.line,
                \ 'column': l:response_item.start.offset,
                \})
            endfor

            if empty(l:item_list)
                call ale#util#Execute('echom ''No references found.''')
            else
                call ale#preview#ShowSelection(l:item_list)
            endif
        endif
    endif
endfunction

function! ale#references#HandleLSPResponse(conn_id, response) abort
    if has_key(a:response, 'id')
    \&& has_key(s:references_map, a:response.id)
        call remove(s:references_map, a:response.id)

        " The result can be a Dictionary item, a List of the same, or null.
        let l:result = get(a:response, 'result', [])
        let l:item_list = []

        for l:response_item in l:result
            call add(l:item_list, {
            \ 'filename': ale#path#FromURI(l:response_item.uri),
            \ 'line': l:response_item.range.start.line + 1,
            \ 'column': l:response_item.range.start.character + 1,
            \})
        endfor

        if empty(l:item_list)
            call ale#util#Execute('echom ''No references found.''')
        else
            call ale#preview#ShowSelection(l:item_list)
        endif
    endif
endfunction

function! s:OnReady(linter, lsp_details, line, column, ...) abort
    let l:buffer = a:lsp_details.buffer
    let l:id = a:lsp_details.connection_id

    let l:Callback = a:linter.lsp is# 'tsserver'
    \   ? function('ale#references#HandleTSServerResponse')
    \   : function('ale#references#HandleLSPResponse')

    call ale#lsp#RegisterCallback(l:id, l:Callback)

    if a:linter.lsp is# 'tsserver'
        let l:message = ale#lsp#tsserver_message#References(
        \   l:buffer,
        \   a:line,
        \   a:column
        \)
    else
        " Send a message saying the buffer has changed first, or the
        " references position probably won't make sense.
        call ale#lsp#NotifyForChanges(l:id, l:buffer)

        let l:message = ale#lsp#message#References(l:buffer, a:line, a:column)
    endif

    let l:request_id = ale#lsp#Send(l:id, l:message)

    let s:references_map[l:request_id] = {}
endfunction

function! s:FindReferences(linter) abort
    let l:buffer = bufnr('')
    let [l:line, l:column] = getcurpos()[1:2]

    if a:linter.lsp isnot# 'tsserver'
        let l:column = min([l:column, len(getline(l:line))])
    endif

    let l:lsp_details = ale#lsp_linter#StartLSP(l:buffer, a:linter)

    if empty(l:lsp_details)
        return 0
    endif

    let l:id = l:lsp_details.connection_id

    call ale#lsp#WaitForCapability(l:id, 'references', function('s:OnReady', [
    \   a:linter, l:lsp_details, l:line, l:column
    \]))
endfunction

function! ale#references#Find() abort
    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            call s:FindReferences(l:linter)
        endif
    endfor
endfunction
