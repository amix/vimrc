let g:ale_default_navigation = get(g:, 'ale_default_navigation', 'buffer')

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

function! ale#references#FormatTSResponseItem(response_item, options) abort
    if get(a:options, 'open_in') is# 'quickfix'
        return {
        \ 'filename': a:response_item.file,
        \ 'lnum': a:response_item.start.line,
        \ 'col': a:response_item.start.offset,
        \}
    else
        return {
        \ 'filename': a:response_item.file,
        \ 'line': a:response_item.start.line,
        \ 'column': a:response_item.start.offset,
        \ 'match': substitute(a:response_item.lineText, '^\s*\(.\{-}\)\s*$', '\1', ''),
        \}
    endif
endfunction

function! ale#references#HandleTSServerResponse(conn_id, response) abort
    if get(a:response, 'command', '') is# 'references'
    \&& has_key(s:references_map, a:response.request_seq)
        let l:options = remove(s:references_map, a:response.request_seq)

        if get(a:response, 'success', v:false) is v:true
            let l:item_list = []

            for l:response_item in a:response.body.refs
                call add(
                \ l:item_list,
                \ ale#references#FormatTSResponseItem(l:response_item, l:options)
                \)
            endfor

            if empty(l:item_list)
                call ale#util#Execute('echom ''No references found.''')
            else
                if get(l:options, 'open_in') is# 'quickfix'
                    call setqflist([], 'r')
                    call setqflist(l:item_list, 'a')
                    call ale#util#Execute('cc 1')
                else
                    call ale#preview#ShowSelection(l:item_list, l:options)
                endif
            endif
        endif
    endif
endfunction

function! ale#references#FormatLSPResponseItem(response_item, options) abort
    if get(a:options, 'open_in') is# 'quickfix'
        return {
        \ 'filename': ale#util#ToResource(a:response_item.uri),
        \ 'lnum': a:response_item.range.start.line + 1,
        \ 'col': a:response_item.range.start.character + 1,
        \}
    else
        return {
        \ 'filename': ale#util#ToResource(a:response_item.uri),
        \ 'line': a:response_item.range.start.line + 1,
        \ 'column': a:response_item.range.start.character + 1,
        \}
    endif
endfunction

function! ale#references#HandleLSPResponse(conn_id, response) abort
    if has_key(a:response, 'id')
    \&& has_key(s:references_map, a:response.id)
        let l:options = remove(s:references_map, a:response.id)

        " The result can be a Dictionary item, a List of the same, or null.
        let l:result = get(a:response, 'result', [])
        let l:item_list = []

        if type(l:result) is v:t_list
            for l:response_item in l:result
                call add(l:item_list,
                \ ale#references#FormatLSPResponseItem(l:response_item, l:options)
                \)
            endfor
        endif

        if empty(l:item_list)
            call ale#util#Execute('echom ''No references found.''')
        else
            if get(l:options, 'open_in') is# 'quickfix'
                call setqflist([], 'r')
                call setqflist(l:item_list, 'a')
                call ale#util#Execute('cc 1')
            else
                call ale#preview#ShowSelection(l:item_list, l:options)
            endif
        endif
    endif
endfunction

function! s:OnReady(line, column, options, linter, lsp_details) abort
    let l:id = a:lsp_details.connection_id

    if !ale#lsp#HasCapability(l:id, 'references')
        return
    endif

    let l:buffer = a:lsp_details.buffer

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

    let s:references_map[l:request_id] = {
    \ 'use_relative_paths': has_key(a:options, 'use_relative_paths') ? a:options.use_relative_paths : 0,
    \ 'open_in': get(a:options, 'open_in', 'current-buffer'),
    \}
endfunction

function! ale#references#Find(...) abort
    let l:options = {}

    if len(a:000) > 0
        for l:option in a:000
            if l:option is? '-relative'
                let l:options.use_relative_paths = 1
            elseif l:option is? '-tab'
                let l:options.open_in = 'tab'
            elseif l:option is? '-split'
                let l:options.open_in = 'split'
            elseif l:option is? '-vsplit'
                let l:options.open_in = 'vsplit'
            elseif l:option is? '-quickfix'
                let l:options.open_in = 'quickfix'
            endif
        endfor
    endif

    if !has_key(l:options, 'open_in')
        let l:default_navigation = ale#Var(bufnr(''), 'default_navigation')

        if index(['tab', 'split', 'vsplit'], l:default_navigation) >= 0
            let l:options.open_in = l:default_navigation
        endif
    endif

    let l:buffer = bufnr('')
    let [l:line, l:column] = getpos('.')[1:2]
    let l:column = min([l:column, len(getline(l:line))])
    let l:Callback = function('s:OnReady', [l:line, l:column, l:options])

    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            call ale#lsp_linter#StartLSP(l:buffer, l:linter, l:Callback)
        endif
    endfor
endfunction
