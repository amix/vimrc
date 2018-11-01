let s:symbol_map = {}

" Used to get the symbol map in tests.
function! ale#symbol#GetMap() abort
    return deepcopy(s:symbol_map)
endfunction

" Used to set the symbol map in tests.
function! ale#symbol#SetMap(map) abort
    let s:symbol_map = a:map
endfunction

function! ale#symbol#ClearLSPData() abort
    let s:symbol_map = {}
endfunction

function! ale#symbol#HandleLSPResponse(conn_id, response) abort
    if has_key(a:response, 'id')
    \&& has_key(s:symbol_map, a:response.id)
        let l:options = remove(s:symbol_map, a:response.id)

        let l:result = get(a:response, 'result', v:null)
        let l:item_list = []

        if type(l:result) is v:t_list
            " Each item looks like this:
            " {
            "   'name': 'foo',
            "   'kind': 123,
            "   'deprecated': v:false,
            "   'location': {
            "     'uri': 'file://...',
            "     'range': {
            "       'start': {'line': 0, 'character': 0},
            "       'end': {'line': 0, 'character': 0},
            "     },
            "   },
            "   'containerName': 'SomeContainer',
            " }
            for l:response_item in l:result
                let l:location = l:response_item.location

                call add(l:item_list, {
                \ 'filename': ale#path#FromURI(l:location.uri),
                \ 'line': l:location.range.start.line + 1,
                \ 'column': l:location.range.start.character + 1,
                \ 'match': l:response_item.name,
                \})
            endfor
        endif

        if empty(l:item_list)
            call ale#util#Execute('echom ''No symbols found.''')
        else
            call ale#preview#ShowSelection(l:item_list)
        endif
    endif
endfunction

function! s:OnReady(linter, lsp_details, query, ...) abort
    let l:buffer = a:lsp_details.buffer

    " If we already made a request, stop here.
    if getbufvar(l:buffer, 'ale_symbol_request_made', 0)
        return
    endif

    let l:id = a:lsp_details.connection_id

    let l:Callback = function('ale#symbol#HandleLSPResponse')
    call ale#lsp#RegisterCallback(l:id, l:Callback)

    let l:message = ale#lsp#message#Symbol(a:query)
    let l:request_id = ale#lsp#Send(l:id, l:message)

    call setbufvar(l:buffer, 'ale_symbol_request_made', 1)
    let s:symbol_map[l:request_id] = {
    \   'buffer': l:buffer,
    \}
endfunction

function! s:Search(linter, buffer, query) abort
    let l:lsp_details = ale#lsp_linter#StartLSP(a:buffer, a:linter)

    if !empty(l:lsp_details)
        call ale#lsp#WaitForCapability(
        \   l:lsp_details.connection_id,
        \   'symbol_search',
        \   function('s:OnReady', [a:linter, l:lsp_details, a:query]),
        \)
    endif
endfunction

function! ale#symbol#Search(query) abort
    if type(a:query) isnot v:t_string || empty(a:query)
        throw 'A non-empty string must be provided!'
    endif

    let l:buffer = bufnr('')

    " Set a flag so we only make one request.
    call setbufvar(l:buffer, 'ale_symbol_request_made', 0)

    for l:linter in ale#linter#Get(getbufvar(l:buffer, '&filetype'))
        if !empty(l:linter.lsp) && l:linter.lsp isnot# 'tsserver'
            call s:Search(l:linter, l:buffer, a:query)
        endif
    endfor
endfunction
