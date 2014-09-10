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
        call syntastic#log#debug(g:SyntasticDebugNotifications, 'cursor: refresh')
        let b:syntastic_messages = copy(a:loclist.messages(bufnr('')))
        let b:oldLine = -1
        autocmd! syntastic CursorMoved
        autocmd syntastic CursorMoved * call g:SyntasticRefreshCursor()
    endif
endfunction " }}}2

" @vimlint(EVL103, 1, a:loclist)
function! g:SyntasticCursorNotifier.reset(loclist) " {{{2
    call syntastic#log#debug(g:SyntasticDebugNotifications, 'cursor: reset')
    autocmd! syntastic CursorMoved
    unlet! b:syntastic_messages
    let b:oldLine = -1
endfunction " }}}2
" @vimlint(EVL103, 0, a:loclist)

" }}}1

" Private methods {{{1

" The following defensive nonsense is needed because of the nature of autocmd
function! g:SyntasticRefreshCursor() " {{{2
    if !exists('b:syntastic_messages') || empty(b:syntastic_messages)
        " file not checked
        return
    endif

    if !exists('b:oldLine')
        let b:oldLine = -1
    endif
    let l = line('.')
    if l == b:oldLine
        return
    endif
    let b:oldLine = l

    if has_key(b:syntastic_messages, l)
        call syntastic#util#wideMsg(b:syntastic_messages[l])
    else
        echo
    endif
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
