" Author: Jerko Steiner <jerko.steiner@gmail.com>
" Description: Rename symbol support for LSP / tsserver

let s:rename_map = {}

" Used to get the rename map in tests.
function! ale#rename#GetMap() abort
    return deepcopy(s:rename_map)
endfunction

" Used to set the rename map in tests.
function! ale#rename#SetMap(map) abort
    let s:rename_map = a:map
endfunction

function! ale#rename#ClearLSPData() abort
    let s:rename_map = {}
endfunction

let g:ale_rename_tsserver_find_in_comments = get(g:, 'ale_rename_tsserver_find_in_comments')
let g:ale_rename_tsserver_find_in_strings = get(g:, 'ale_rename_tsserver_find_in_strings')

function! s:message(message) abort
    call ale#util#Execute('echom ' . string(a:message))
endfunction

function! ale#rename#HandleTSServerResponse(conn_id, response) abort
    if get(a:response, 'command', '') isnot# 'rename'
        return
    endif

    if !has_key(s:rename_map, a:response.request_seq)
        return
    endif

    let l:options = remove(s:rename_map, a:response.request_seq)

    let l:old_name = l:options.old_name
    let l:new_name = l:options.new_name

    if get(a:response, 'success', v:false) is v:false
        let l:message = get(a:response, 'message', 'unknown')
        call s:message('Error renaming "' . l:old_name . '" to: "' . l:new_name
        \ . '". Reason: ' . l:message)

        return
    endif

    let l:changes = []

    for l:response_item in a:response.body.locs
        let l:filename = l:response_item.file
        let l:text_changes = []

        for l:loc in l:response_item.locs
            call add(l:text_changes, {
            \ 'start': {
            \   'line': l:loc.start.line,
            \   'offset': l:loc.start.offset,
            \ },
            \ 'end': {
            \   'line': l:loc.end.line,
            \   'offset': l:loc.end.offset,
            \ },
            \ 'newText': l:new_name,
            \})
        endfor

        call add(l:changes, {
        \   'fileName': l:filename,
        \   'textChanges': l:text_changes,
        \})
    endfor

    if empty(l:changes)
        call s:message('Error renaming "' . l:old_name . '" to: "' . l:new_name . '"')

        return
    endif

    call ale#code_action#HandleCodeAction(
    \   {
    \       'description': 'rename',
    \       'changes': l:changes,
    \   },
    \   {
    \       'conn_id': a:conn_id,
    \       'should_save': !&hidden,
    \   },
    \)
endfunction

function! ale#rename#HandleLSPResponse(conn_id, response) abort
    if has_key(a:response, 'id')
    \&& has_key(s:rename_map, a:response.id)
        let l:options = remove(s:rename_map, a:response.id)

        if !has_key(a:response, 'result')
            call s:message('No rename result received from server')

            return
        endif

        let l:changes_map = ale#code_action#GetChanges(a:response.result)

        if empty(l:changes_map)
            call s:message('No changes received from server')

            return
        endif

        let l:changes = ale#code_action#BuildChangesList(l:changes_map)

        call ale#code_action#HandleCodeAction(
        \   {
        \       'description': 'rename',
        \       'changes': l:changes,
        \   },
        \   {
        \       'conn_id': a:conn_id,
        \       'should_save': !&hidden,
        \   },
        \)
    endif
endfunction

function! s:OnReady(line, column, options, linter, lsp_details) abort
    let l:id = a:lsp_details.connection_id

    if !ale#lsp#HasCapability(l:id, 'rename')
        return
    endif

    let l:buffer = a:lsp_details.buffer

    let l:Callback = a:linter.lsp is# 'tsserver'
    \   ? function('ale#rename#HandleTSServerResponse')
    \   : function('ale#rename#HandleLSPResponse')

    call ale#lsp#RegisterCallback(l:id, l:Callback)

    if a:linter.lsp is# 'tsserver'
        let l:message = ale#lsp#tsserver_message#Rename(
        \   l:buffer,
        \   a:line,
        \   a:column,
        \   g:ale_rename_tsserver_find_in_comments,
        \   g:ale_rename_tsserver_find_in_strings,
        \)
    else
        " Send a message saying the buffer has changed first, or the
        " rename position probably won't make sense.
        call ale#lsp#NotifyForChanges(l:id, l:buffer)

        let l:message = ale#lsp#message#Rename(
        \   l:buffer,
        \   a:line,
        \   a:column,
        \   a:options.new_name
        \)
    endif

    let l:request_id = ale#lsp#Send(l:id, l:message)

    let s:rename_map[l:request_id] = a:options
endfunction

function! s:ExecuteRename(linter, options) abort
    let l:buffer = bufnr('')
    let [l:line, l:column] = getpos('.')[1:2]

    if a:linter.lsp isnot# 'tsserver'
        let l:column = min([l:column, len(getline(l:line))])
    endif

    let l:Callback = function('s:OnReady', [l:line, l:column, a:options])
    call ale#lsp_linter#StartLSP(l:buffer, a:linter, l:Callback)
endfunction

function! ale#rename#Execute() abort
    let l:lsp_linters = []

    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            call add(l:lsp_linters, l:linter)
        endif
    endfor

    if empty(l:lsp_linters)
        call s:message('No active LSPs')

        return
    endif

    let l:old_name = expand('<cword>')
    let l:new_name = ale#util#Input('New name: ', l:old_name)

    if empty(l:new_name)
        call s:message('New name cannot be empty!')

        return
    endif

    for l:lsp_linter in l:lsp_linters
        call s:ExecuteRename(l:lsp_linter, {
        \   'old_name': l:old_name,
        \   'new_name': l:new_name,
        \})
    endfor
endfunction
