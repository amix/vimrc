if exists("g:loaded_syntastic_notifier_cursor") || !exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_notifier_cursor = 1

let g:SyntasticCursorNotifier = {}

" Public methods {{{1

function! g:SyntasticCursorNotifier.New() " {{{2
    let newObj = copy(self)
    return newObj
endfunction " }}}2

function! g:SyntasticCursorNotifier.enabled() " {{{2
    return syntastic#util#var('echo_current_error')
endfunction " }}}2

function! g:SyntasticCursorNotifier.refresh(loclist) " {{{2
    if self.enabled() && !a:loclist.isEmpty()
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'cursor: refresh')
        let b:syntastic_private_messages = copy(a:loclist.messages(bufnr('')))
        let b:syntastic_private_line = -1
        let b:syntastic_cursor_columns = a:loclist.getCursorColumns()
        autocmd! syntastic CursorMoved
        autocmd syntastic CursorMoved * call SyntasticRefreshCursor()
    endif
endfunction " }}}2

" @vimlint(EVL103, 1, a:loclist)
function! g:SyntasticCursorNotifier.reset(loclist) " {{{2
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'cursor: reset')
    autocmd! syntastic CursorMoved
    unlet! b:syntastic_private_messages
    let b:syntastic_private_line = -1
endfunction " }}}2
" @vimlint(EVL103, 0, a:loclist)

" }}}1

" Private functions {{{1

function! SyntasticRefreshCursor() " {{{2
    if !exists('b:syntastic_private_messages') || empty(b:syntastic_private_messages)
        " file not checked
        return
    endif

    if !exists('b:syntastic_private_line')
        let b:syntastic_private_line = -1
    endif
    let l = line('.')
    let current_messages = get(b:syntastic_private_messages, l, {})

    if !exists('b:syntastic_cursor_columns')
        let b:syntastic_cursor_columns = g:syntastic_cursor_columns
    endif

    if b:syntastic_cursor_columns
        let c = virtcol('.')
        if !exists('b:syntastic_private_idx')
            let b:syntastic_private_idx = -1
        endif

        if s:_is_same_index(l, b:syntastic_private_line, c, b:syntastic_private_idx, current_messages)
            return
        else
            let b:syntastic_private_line = l
        endif

        if !empty(current_messages)
            let b:syntastic_private_idx = s:_find_index(c, current_messages)
            call syntastic#util#wideMsg(current_messages[b:syntastic_private_idx].text)
        else
            let b:syntastic_private_idx = -1
            echo
        endif
    else
        if l == b:syntastic_private_line
            return
        endif
        let b:syntastic_private_line = l

        if !empty(current_messages)
            call syntastic#util#wideMsg(current_messages[0].text)
        else
            echo
        endif
    endif
endfunction " }}}2

" }}}1

" Utilities {{{1

function! s:_is_same_index(line, old_line, column, idx, messages) " {{{2
    if a:old_line >= 0 && a:line == a:old_line && a:idx >= 0
        if len(a:messages) <= 1
            return 1
        endif

        if a:messages[a:idx].scol <= a:column || a:idx == 0
            if a:idx == len(a:messages) - 1 || a:column < a:messages[a:idx + 1].scol
                return 1
            else
                return 0
            endif
        else
            return 0
        endif
    else
        return 0
    endif
endfunction " }}}2

function! s:_find_index(column, messages) " {{{2
    let max = len(a:messages) - 1
    if max == 0
        return 0
    endif
    let min = 0

    " modified binary search: assign index 0 to columns to the left of the first error
    while min < max - 1
        let mid = (min + max) / 2
        if a:column < a:messages[mid].scol
            let max = mid
        else
            let min = mid
        endif
    endwhile

    return a:column < a:messages[max].scol ? min : max
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
