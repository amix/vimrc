" Author: w0rp <devw0rp@gmail.com>
" Description: Go to definition support for LSP linters.

let s:go_to_definition_map = {}

" Used to get the definition map in tests.
function! ale#definition#GetMap() abort
    return deepcopy(s:go_to_definition_map)
endfunction

" Used to set the definition map in tests.
function! ale#definition#SetMap(map) abort
    let s:go_to_definition_map = a:map
endfunction

function! ale#definition#ClearLSPData() abort
    let s:go_to_definition_map = {}
endfunction

function! ale#definition#HandleTSServerResponse(conn_id, response) abort
    if get(a:response, 'command', '') is# 'definition'
    \&& has_key(s:go_to_definition_map, a:response.request_seq)
        let l:options = remove(s:go_to_definition_map, a:response.request_seq)

        if get(a:response, 'success', v:false) is v:true && !empty(a:response.body)
            let l:filename = a:response.body[0].file
            let l:line = a:response.body[0].start.line
            let l:column = a:response.body[0].start.offset

            call ale#util#Open(l:filename, l:line, l:column, l:options)
        endif
    endif
endfunction

function! ale#definition#HandleLSPResponse(conn_id, response) abort
    if has_key(a:response, 'id')
    \&& has_key(s:go_to_definition_map, a:response.id)
        let l:options = remove(s:go_to_definition_map, a:response.id)

        " The result can be a Dictionary item, a List of the same, or null.
        let l:result = get(a:response, 'result', v:null)

        if type(l:result) is v:t_dict
            let l:result = [l:result]
        elseif type(l:result) isnot v:t_list
            let l:result = []
        endif

        for l:item in l:result
            let l:filename = ale#path#FromURI(l:item.uri)
            let l:line = l:item.range.start.line + 1
            let l:column = l:item.range.start.character + 1

            call ale#util#Open(l:filename, l:line, l:column, l:options)
            break
        endfor
    endif
endfunction

function! s:OnReady(linter, lsp_details, line, column, options, ...) abort
    let l:buffer = a:lsp_details.buffer
    let l:id = a:lsp_details.connection_id

    let l:Callback = a:linter.lsp is# 'tsserver'
    \   ? function('ale#definition#HandleTSServerResponse')
    \   : function('ale#definition#HandleLSPResponse')
    call ale#lsp#RegisterCallback(l:id, l:Callback)

    if a:linter.lsp is# 'tsserver'
        let l:message = ale#lsp#tsserver_message#Definition(
        \   l:buffer,
        \   a:line,
        \   a:column
        \)
    else
        " Send a message saying the buffer has changed first, or the
        " definition position probably won't make sense.
        call ale#lsp#NotifyForChanges(l:id, l:buffer)

        " For LSP completions, we need to clamp the column to the length of
        " the line. python-language-server and perhaps others do not implement
        " this correctly.
        let l:message = ale#lsp#message#Definition(l:buffer, a:line, a:column)
    endif

    let l:request_id = ale#lsp#Send(l:id, l:message)

    let s:go_to_definition_map[l:request_id] = {
    \   'open_in_tab': get(a:options, 'open_in_tab', 0),
    \}
endfunction

function! s:GoToLSPDefinition(linter, options) abort
    let l:buffer = bufnr('')
    let [l:line, l:column] = getcurpos()[1:2]
    let l:lsp_details = ale#lsp_linter#StartLSP(l:buffer, a:linter)

    if a:linter.lsp isnot# 'tsserver'
        let l:column = min([l:column, len(getline(l:line))])
    endif

    if empty(l:lsp_details)
        return 0
    endif

    let l:id = l:lsp_details.connection_id

    call ale#lsp#WaitForCapability(l:id, 'definition', function('s:OnReady', [
    \   a:linter, l:lsp_details, l:line, l:column, a:options
    \]))
endfunction

function! ale#definition#GoTo(options) abort
    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            call s:GoToLSPDefinition(l:linter, a:options)
        endif
    endfor
endfunction
