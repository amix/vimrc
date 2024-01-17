" Author: Dalius Dobravolskas <dalius.dobravolskas@gmail.com>
" Description: Rename file support for tsserver

let s:filerename_map = {}

" Used to get the rename map in tests.
function! ale#filerename#GetMap() abort
    return deepcopy(s:filerename_map)
endfunction

" Used to set the rename map in tests.
function! ale#filerename#SetMap(map) abort
    let s:filerename_map = a:map
endfunction

function! ale#filerename#ClearLSPData() abort
    let s:filerename_map = {}
endfunction

function! s:message(message) abort
    call ale#util#Execute('echom ' . string(a:message))
endfunction

function! ale#filerename#HandleTSServerResponse(conn_id, response) abort
    if get(a:response, 'command', '') isnot# 'getEditsForFileRename'
        return
    endif

    if !has_key(s:filerename_map, a:response.request_seq)
        return
    endif

    let l:options = remove(s:filerename_map, a:response.request_seq)

    let l:old_name = l:options.old_name
    let l:new_name = l:options.new_name

    if get(a:response, 'success', v:false) is v:false
        let l:message = get(a:response, 'message', 'unknown')
        call s:message('Error renaming file "' . l:old_name . '" to "' . l:new_name
        \ . '". Reason: ' . l:message)

        return
    endif

    let l:changes = a:response.body

    if empty(l:changes)
        call s:message('No changes while renaming "' . l:old_name . '" to "' . l:new_name . '"')
    else
        call ale#code_action#HandleCodeAction(
        \   {
        \       'description': 'filerename',
        \       'changes': l:changes,
        \   },
        \   {
        \       'should_save': 1,
        \   },
        \)
    endif

    silent! noautocmd execute 'saveas ' . l:new_name
    call delete(l:old_name)
endfunction

function! s:OnReady(options, linter, lsp_details) abort
    let l:id = a:lsp_details.connection_id

    if !ale#lsp#HasCapability(l:id, 'filerename')
        return
    endif

    let l:buffer = a:lsp_details.buffer

    let l:Callback = function('ale#filerename#HandleTSServerResponse')

    call ale#lsp#RegisterCallback(l:id, l:Callback)

    let l:message = ale#lsp#tsserver_message#GetEditsForFileRename(
    \   a:options.old_name,
    \   a:options.new_name,
    \)

    let l:request_id = ale#lsp#Send(l:id, l:message)

    let s:filerename_map[l:request_id] = a:options
endfunction

function! s:ExecuteFileRename(linter, options) abort
    let l:buffer = bufnr('')

    let l:Callback = function('s:OnReady', [a:options])
    call ale#lsp_linter#StartLSP(l:buffer, a:linter, l:Callback)
endfunction

function! ale#filerename#Execute() abort
    let l:buffer = bufnr('')
    let l:lsp_linters = []

    for l:linter in ale#lsp_linter#GetEnabled(l:buffer)
        if l:linter.lsp is# 'tsserver'
            call add(l:lsp_linters, l:linter)
        endif
    endfor

    if empty(l:lsp_linters)
        call s:message('No active tsserver LSPs')

        return
    endif

    let l:old_name = expand('#' . l:buffer . ':p')
    let l:new_name = ale#util#Input('New file name: ', l:old_name, 'file')

    if l:old_name is# l:new_name
        call s:message('New file name matches old file name')

        return
    endif

    if empty(l:new_name)
        call s:message('New name cannot be empty!')

        return
    endif

    for l:lsp_linter in l:lsp_linters
        call s:ExecuteFileRename(l:lsp_linter, {
        \   'old_name': l:old_name,
        \   'new_name': l:new_name,
        \})
    endfor
endfunction
