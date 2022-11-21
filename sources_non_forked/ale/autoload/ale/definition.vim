" Author: w0rp <devw0rp@gmail.com>
" Description: Go to definition support for LSP linters.

let s:go_to_definition_map = {}

" Enable automatic updates of the tagstack
let g:ale_update_tagstack = get(g:, 'ale_update_tagstack', 1)
let g:ale_default_navigation = get(g:, 'ale_default_navigation', 'buffer')

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

function! ale#definition#UpdateTagStack() abort
    let l:should_update_tagstack = exists('*gettagstack') && exists('*settagstack') && g:ale_update_tagstack

    if l:should_update_tagstack
        " Grab the old location (to jump back to) and the word under the
        " cursor (as a label for the tagstack)
        let l:old_location = [bufnr('%'), line('.'), col('.'), 0]
        let l:tagname = expand('<cword>')
        let l:winid = win_getid()
        call settagstack(l:winid, {'items': [{'from': l:old_location, 'tagname': l:tagname}]}, 'a')
        call settagstack(l:winid, {'curidx': len(gettagstack(l:winid)['items']) + 1})
    endif
endfunction

function! ale#definition#HandleTSServerResponse(conn_id, response) abort
    if has_key(a:response, 'request_seq')
    \&& has_key(s:go_to_definition_map, a:response.request_seq)
        let l:options = remove(s:go_to_definition_map, a:response.request_seq)

        if get(a:response, 'success', v:false) is v:true && !empty(a:response.body)
            let l:filename = a:response.body[0].file
            let l:line = a:response.body[0].start.line
            let l:column = a:response.body[0].start.offset

            call ale#definition#UpdateTagStack()
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
            if has_key(l:item, 'targetUri')
                " LocationLink items use targetUri
                let l:uri = l:item.targetUri
                let l:line = l:item.targetRange.start.line + 1
                let l:column = l:item.targetRange.start.character + 1
            else
                " LocationLink items use uri
                let l:uri = l:item.uri
                let l:line = l:item.range.start.line + 1
                let l:column = l:item.range.start.character + 1
            endif

            call ale#definition#UpdateTagStack()

            let l:uri_handler = ale#uri#GetURIHandler(l:uri)

            if l:uri_handler is# v:null
                let l:filename = ale#path#FromFileURI(l:uri)
                call ale#util#Open(l:filename, l:line, l:column, l:options)
            else
                call l:uri_handler.OpenURILink(l:uri, l:line, l:column, l:options, a:conn_id)
            endif

            break
        endfor
    endif
endfunction

function! s:OnReady(line, column, options, capability, linter, lsp_details) abort
    let l:id = a:lsp_details.connection_id

    if !ale#lsp#HasCapability(l:id, a:capability)
        return
    endif

    let l:buffer = a:lsp_details.buffer

    let l:Callback = a:linter.lsp is# 'tsserver'
    \   ? function('ale#definition#HandleTSServerResponse')
    \   : function('ale#definition#HandleLSPResponse')
    call ale#lsp#RegisterCallback(l:id, l:Callback)

    if a:linter.lsp is# 'tsserver'
        if a:capability is# 'definition'
            let l:message = ale#lsp#tsserver_message#Definition(
            \   l:buffer,
            \   a:line,
            \   a:column
            \)
        elseif a:capability is# 'typeDefinition'
            let l:message = ale#lsp#tsserver_message#TypeDefinition(
            \   l:buffer,
            \   a:line,
            \   a:column
            \)
        elseif a:capability is# 'implementation'
            let l:message = ale#lsp#tsserver_message#Implementation(
            \   l:buffer,
            \   a:line,
            \   a:column
            \)
        endif
    else
        " Send a message saying the buffer has changed first, or the
        " definition position probably won't make sense.
        call ale#lsp#NotifyForChanges(l:id, l:buffer)

        " For LSP completions, we need to clamp the column to the length of
        " the line. python-language-server and perhaps others do not implement
        " this correctly.
        if a:capability is# 'definition'
            let l:message = ale#lsp#message#Definition(l:buffer, a:line, a:column)
        elseif a:capability is# 'typeDefinition'
            let l:message = ale#lsp#message#TypeDefinition(l:buffer, a:line, a:column)
        elseif a:capability is# 'implementation'
            let l:message = ale#lsp#message#Implementation(l:buffer, a:line, a:column)
        else
            " XXX: log here?
            return
        endif
    endif

    let l:request_id = ale#lsp#Send(l:id, l:message)

    let s:go_to_definition_map[l:request_id] = {
    \   'open_in': get(a:options, 'open_in', 'current-buffer'),
    \}
endfunction

function! s:GoToLSPDefinition(linter, options, capability) abort
    let l:buffer = bufnr('')
    let [l:line, l:column] = getpos('.')[1:2]
    let l:column = min([l:column, len(getline(l:line))])

    let l:Callback = function(
    \   's:OnReady',
    \   [l:line, l:column, a:options, a:capability]
    \)
    call ale#lsp_linter#StartLSP(l:buffer, a:linter, l:Callback)
endfunction

function! ale#definition#GoTo(options) abort
    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            call s:GoToLSPDefinition(l:linter, a:options, 'definition')
        endif
    endfor
endfunction

function! ale#definition#GoToType(options) abort
    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            call s:GoToLSPDefinition(l:linter, a:options, 'typeDefinition')
        endif
    endfor
endfunction

function! ale#definition#GoToImpl(options) abort
    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            call s:GoToLSPDefinition(l:linter, a:options, 'implementation')
        endif
    endfor
endfunction

function! ale#definition#GoToCommandHandler(command, ...) abort
    let l:options = {}

    if len(a:000) > 0
        for l:option in a:000
            if l:option is? '-tab'
                let l:options.open_in = 'tab'
            elseif l:option is? '-split'
                let l:options.open_in = 'split'
            elseif l:option is? '-vsplit'
                let l:options.open_in = 'vsplit'
            endif
        endfor
    endif

    if !has_key(l:options, 'open_in')
        let l:default_navigation = ale#Var(bufnr(''), 'default_navigation')

        if index(['tab', 'split', 'vsplit'], l:default_navigation) >= 0
            let l:options.open_in = l:default_navigation
        endif
    endif

    if a:command is# 'type'
        call ale#definition#GoToType(l:options)
    elseif a:command is# 'implementation'
        call ale#definition#GoToImpl(l:options)
    else
        call ale#definition#GoTo(l:options)
    endif
endfunction
