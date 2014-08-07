if exists("g:loaded_syntastic_notifiers") || !exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_notifiers = 1

let g:SyntasticNotifiers = {}

let s:notifier_types = ['signs', 'balloons', 'highlighting', 'cursor', 'autoloclist']
lockvar! s:notifier_types

let s:persistent_notifiers = ['signs', 'balloons']
lockvar! s:persistent_notifiers

" Public methods {{{1

function! g:SyntasticNotifiers.Instance() " {{{2
    if !exists('s:SyntasticNotifiersInstance')
        let s:SyntasticNotifiersInstance = copy(self)
        call s:SyntasticNotifiersInstance._initNotifiers()
    endif

    return s:SyntasticNotifiersInstance
endfunction " }}}2

function! g:SyntasticNotifiers.refresh(loclist) " {{{2
    if !a:loclist.isEmpty() && !a:loclist.isNewerThan([])
        " loclist not fully constructed yet
        return
    endif

    call syntastic#log#debug(g:SyntasticDebugNotifications, 'notifiers: refresh')
    for type in self._enabled_types
        let class = substitute(type, '\m.*', 'Syntastic\u&Notifier', '')
        if !has_key(g:{class}, 'enabled') || self._notifier[type].enabled()
            if index(s:persistent_notifiers, type) > -1
                " refresh only if loclist has changed since last call
                if !exists('b:syntastic_' . type . '_stamp')
                    let b:syntastic_{type}_stamp = []
                endif
                if a:loclist.isNewerThan(b:syntastic_{type}_stamp) || a:loclist.isEmpty()
                    call self._notifier[type].refresh(a:loclist)
                    let b:syntastic_{type}_stamp = syntastic#util#stamp()
                endif
            else
                call self._notifier[type].refresh(a:loclist)
            endif
        endif
    endfor
endfunction " }}}2

function! g:SyntasticNotifiers.reset(loclist) " {{{2
    call syntastic#log#debug(g:SyntasticDebugNotifications, 'notifiers: reset')
    for type in self._enabled_types
        let class = substitute(type, '\m.*', 'Syntastic\u&Notifier', '')

        " reset notifiers regardless if they are enabled or not, since
        " the user might have disabled them since the last refresh();
        " notifiers MUST be prepared to deal with reset() when disabled
        if has_key(g:{class}, 'reset')
            call self._notifier[type].reset(a:loclist)
        endif

        " also reset stamps
        if index(s:persistent_notifiers, type) > -1
            let b:syntastic_{type}_stamp = []
        endif
    endfor
endfunction " }}}2

" }}}1

" Private methods {{{1

function! g:SyntasticNotifiers._initNotifiers() " {{{2
    let self._notifier = {}
    for type in s:notifier_types
        let class = substitute(type, '\m.*', 'Syntastic\u&Notifier', '')
        let self._notifier[type] = g:{class}.New()
    endfor

    let self._enabled_types = copy(s:notifier_types)
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
