" Author: Bjorn Neergaard <bjorn@neersighted.com>, modified by Yann fery <yann@fery.me>
" Description: Manages the loclist and quickfix lists

" This flag dictates if ale open the configured loclist
let g:ale_open_list = get(g:, 'ale_open_list', 0)
" This flag dictates if ale keeps open loclist even if there is no error in loclist
let g:ale_keep_list_window_open = get(g:, 'ale_keep_list_window_open', 0)
" This flag dictates that quickfix windows should be opened vertically
let g:ale_list_vertical = get(g:, 'ale_list_vertical', 0)
" The window size to set for the quickfix and loclist windows
let g:ale_list_window_size = get(g:, 'ale_list_window_size', 10)
" A string format for the loclist messages.
let g:ale_loclist_msg_format = get(g:, 'ale_loclist_msg_format',
\   get(g:, 'ale_echo_msg_format', '%code: %%s')
\)

if !exists('s:timer_args')
    let s:timer_args = {}
endif

" Return 1 if there is a buffer with buftype == 'quickfix' in buffer list
function! ale#list#IsQuickfixOpen() abort
    let l:res = getqflist({ 'winid' : winnr() })

    if has_key(l:res, 'winid') && l:res.winid > 0
        return 1
    endif

    let l:res = getloclist(0, { 'winid' : winnr() })

    if has_key(l:res, 'winid') && l:res.winid > 0
        return 1
    endif

    return 0
endfunction

" Check if we should open the list, based on the save event being fired, and
" that setting being on, or that the error count is at least as high as the
" setting when set to an integer value.
function! s:ShouldOpen(buffer, loclist_len) abort
    let l:val = ale#Var(a:buffer, 'open_list')
    let l:saved = getbufvar(a:buffer, 'ale_save_event_fired', 0)

    return l:val > 0 ? a:loclist_len >= l:val : l:val is# 'on_save' && l:saved
endfunction

" Check if we should close the list, based on the save event being fired, and
" that setting being on, or the setting just being set to an integer value.
function! s:ShouldClose(buffer) abort
    let l:val = ale#Var(a:buffer, 'open_list')
    let l:saved = getbufvar(a:buffer, 'ale_save_event_fired', 0)

    return !((l:val >= 1) || (l:val is# 'on_save' && l:saved))
endfunction

function! s:Deduplicate(list) abort
    let l:list = a:list

    call sort(l:list, function('ale#util#LocItemCompareWithText'))
    call uniq(l:list, function('ale#util#LocItemCompareWithText'))

    return l:list
endfunction

function! ale#list#GetCombinedList() abort
    let l:list = []

    for l:info in values(g:ale_buffer_info)
        call extend(l:list, l:info.loclist)
    endfor

    return s:Deduplicate(l:list)
endfunction

function! s:FixList(buffer, list) abort
    let l:format = ale#Var(a:buffer, 'loclist_msg_format')
    let l:new_list = []

    for l:item in a:list
        let l:fixed_item = copy(l:item)

        let l:fixed_item.text = ale#GetLocItemMessage(l:item, l:format)

        if l:item.bufnr == -1
            " If the buffer number is invalid, remove it.
            call remove(l:fixed_item, 'bufnr')
        endif

        call add(l:new_list, l:fixed_item)
    endfor

    return l:new_list
endfunction

function! s:WinFindBuf(buffer) abort
    return exists('*win_findbuf') ? win_findbuf(str2nr(a:buffer)) : [0]
endfunction

function! s:SetListsImpl(timer_id, buffer, loclist) abort
    let l:title = expand('#' . a:buffer . ':p')

    if g:ale_set_quickfix
        let l:quickfix_list = ale#list#GetCombinedList()

        if has('nvim')
            call setqflist(s:FixList(a:buffer, l:quickfix_list), ' ', l:title)
        else
            call setqflist(s:FixList(a:buffer, l:quickfix_list))
            call setqflist([], 'r', {'title': l:title})
        endif
    elseif g:ale_set_loclist
        " If windows support is off, win_findbuf() may not exist.
        " We'll set result in the current window, which might not be correct,
        " but it's better than nothing.
        let l:ids = s:WinFindBuf(a:buffer)

        let l:loclist = s:Deduplicate(a:loclist)

        for l:id in l:ids
            if has('nvim')
                call setloclist(l:id, s:FixList(a:buffer, l:loclist), ' ', l:title)
            else
                call setloclist(l:id, s:FixList(a:buffer, l:loclist))
                call setloclist(l:id, [], 'r', {'title': l:title})
            endif
        endfor
    endif

    " Save the current view before opening/closing any window
    call setbufvar(a:buffer, 'ale_winview', winsaveview())

    " Open a window to show the problems if we need to.
    "
    " ShouldOpen() checks if the current buffer has enough problems to be
    " opened.
    if s:ShouldOpen(a:buffer, len(a:loclist))
        let l:winnr = winnr()
        let l:mode = mode()

        " open windows vertically instead of default horizontally
        let l:open_type = ''

        if ale#Var(a:buffer, 'list_vertical') == 1
            let l:open_type = 'vert rightbelow '
        endif

        if g:ale_set_quickfix
            if !ale#list#IsQuickfixOpen()
                silent! execute l:open_type . 'copen ' . str2nr(ale#Var(a:buffer, 'list_window_size'))
            endif
        elseif g:ale_set_loclist
            silent! execute l:open_type . 'lopen ' . str2nr(ale#Var(a:buffer, 'list_window_size'))
        endif

        " If focus changed, restore it (jump to the last window).
        if l:winnr isnot# winnr()
            wincmd p
        endif

        " Return to original mode when applicable
        if mode() != l:mode
            if l:mode is? 'v' || l:mode is# "\<c-v>"
                " Reset our last visual selection
                normal! gv
            elseif l:mode is? 's' || l:mode is# "\<c-s>"
                " Reset our last character selection
                normal! "\<c-g>"
            endif
        endif

        call s:RestoreViewIfNeeded(a:buffer)
    endif

    " If ALE isn't currently checking for more problems, close the window if
    " needed now. This check happens inside of this timer function, so
    " the window can be closed reliably.
    if !ale#engine#IsCheckingBuffer(a:buffer)
        call s:CloseWindowIfNeeded(a:buffer)
    endif
endfunction

" Try to restore the window view after closing any of the lists to avoid making
" the it moving around, especially useful when on insert mode
function! s:RestoreViewIfNeeded(buffer) abort
    let l:saved_view = getbufvar(a:buffer, 'ale_winview', {})

    " Saved view is empty, can't do anything
    if empty(l:saved_view)
        return
    endif

    " Check whether the cursor has moved since linting was actually requested. If
    " the user has indeed moved lines, do nothing
    let l:current_view = winsaveview()

    if l:current_view['lnum'] != l:saved_view['lnum']
        return
    endif

    " Anchor view by topline if the list is set to open horizontally
    if ale#Var(a:buffer, 'list_vertical') == 0
        call winrestview({'topline': l:saved_view['topline']})
    endif
endfunction

function! ale#list#SetLists(buffer, loclist) abort
    if get(g:, 'ale_set_lists_synchronously') == 1
    \|| getbufvar(a:buffer, 'ale_save_event_fired', 0)
        " Update lists immediately if running a test synchronously, or if the
        " buffer was saved.
        "
        " The lists need to be updated immediately when saving a buffer so
        " that we can reliably close window automatically, if so configured.
        call s:SetListsImpl(-1, a:buffer, a:loclist)
    else
        call ale#util#StartPartialTimer(
        \   0,
        \   function('s:SetListsImpl'),
        \   [a:buffer, a:loclist],
        \)
    endif
endfunction

function! ale#list#ForcePopulateErrorList(populate_quickfix) abort
    let l:quickfix_bak = g:ale_set_quickfix
    let g:ale_set_quickfix = a:populate_quickfix
    let l:loclist_bak = g:ale_set_loclist
    let g:ale_set_loclist = !a:populate_quickfix
    let l:open_list_bak = g:ale_open_list
    let g:ale_open_list = 1

    let l:buffer = bufnr('')
    let l:loclist = get(g:ale_buffer_info, l:buffer, {'loclist': []}).loclist
    call s:SetListsImpl(-1, l:buffer, l:loclist)

    let g:ale_open_list = l:open_list_bak
    let g:ale_set_loclist = l:loclist_bak
    let g:ale_set_quickfix = l:quickfix_bak
endfunction

function! s:CloseWindowIfNeeded(buffer) abort
    if ale#Var(a:buffer, 'keep_list_window_open') || s:ShouldClose(a:buffer)
        return
    endif

    let l:did_close_any_list = 0

    try
        " Only close windows if the quickfix list or loclist is completely empty,
        " including errors set through other means.
        if g:ale_set_quickfix
            if empty(getqflist())
                cclose
                let l:did_close_any_list = 1
            endif
        else
            let l:win_ids = s:WinFindBuf(a:buffer)

            for l:win_id in l:win_ids
                if g:ale_set_loclist && empty(getloclist(l:win_id))
                    lclose
                    let l:did_close_any_list = 1
                endif
            endfor
        endif
    " Ignore 'Cannot close last window' errors.
    catch /E444/
    endtry

    if l:did_close_any_list
        call s:RestoreViewIfNeeded(a:buffer)
    endif
endfunction
