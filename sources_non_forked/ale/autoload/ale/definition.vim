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

        if type(l:result) is type({})
            let l:result = [l:result]
        elseif type(l:result) isnot type([])
            let l:result = []
        endif

        for l:item in l:result
            let l:filename = ale#path#FromURI(l:item.uri)
            let l:line = l:item.range.start.line + 1
            let l:column = l:item.range.start.character

            call ale#util#Open(l:filename, l:line, l:column, l:options)
            break
        endfor
    endif
endfunction

function! s:GoToLSPDefinition(linter, options) abort
    let l:buffer = bufnr('')
    let [l:line, l:column] = getcurpos()[1:2]

    let l:Callback = a:linter.lsp is# 'tsserver'
    \   ? function('ale#definition#HandleTSServerResponse')
    \   : function('ale#definition#HandleLSPResponse')

    let l:lsp_details = ale#lsp_linter#StartLSP(l:buffer, a:linter, l:Callback)

    if empty(l:lsp_details)
        return 0
    endif

    let l:id = l:lsp_details.connection_id

    if a:linter.lsp is# 'tsserver'
        let l:message = ale#lsp#tsserver_message#Definition(
        \   l:buffer,
        \   l:line,
        \   l:column
        \)
    else
        " Send a message saying the buffer has changed first, or the
        " definition position probably won't make sense.
        call ale#lsp#NotifyForChanges(l:lsp_details)

        let l:column = min([l:column, len(getline(l:line))])

        " For LSP completions, we need to clamp the column to the length of
        " the line. python-language-server and perhaps others do not implement
        " this correctly.
        let l:message = ale#lsp#message#Definition(l:buffer, l:line, l:column)
    endif

    let l:request_id = ale#lsp#Send(l:id, l:message, l:lsp_details.project_root)

    let s:go_to_definition_map[l:request_id] = {
    \   'open_in_tab': get(a:options, 'open_in_tab', 0),
    \}
endfunction

function! ale#definition#GoTo(options) abort
    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            call s:GoToLSPDefinition(l:linter, a:options)
        endif
    endfor
endfunction
