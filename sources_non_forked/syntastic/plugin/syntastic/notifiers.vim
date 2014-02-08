if exists("g:loaded_syntastic_notifiers")
    finish
endif
let g:loaded_syntastic_notifiers = 1

let g:SyntasticNotifiers = {}

let s:notifier_types = ['signs', 'balloons', 'highlighting', 'cursor', 'autoloclist']

" Public methods {{{1

function! g:SyntasticNotifiers.Instance()
    if !exists('s:SyntasticNotifiersInstance')
        let s:SyntasticNotifiersInstance = copy(self)
        call s:SyntasticNotifiersInstance._initNotifiers()
    endif

    return s:SyntasticNotifiersInstance
endfunction

function! g:SyntasticNotifiers.refresh(loclist)
    call syntastic#log#debug(g:SyntasticDebugNotifications, 'notifiers: refresh')
    for type in self._enabled_types
        let class = substitute(type, '\m.*', 'Syntastic\u&Notifier', '')
        if !has_key(g:{class}, 'enabled') || self._notifier[type].enabled()
            call self._notifier[type].refresh(a:loclist)
        endif
    endfor
endfunction

function! g:SyntasticNotifiers.reset(loclist)
    call syntastic#log#debug(g:SyntasticDebugNotifications, 'notifiers: reset')
    for type in self._enabled_types
        let class = substitute(type, '\m.*', 'Syntastic\u&Notifier', '')

        " reset notifiers regardless if they are enabled or not, since
        " the user might have disabled them since the last refresh();
        " notifiers MUST be prepared to deal with reset() when disabled
        if has_key(g:{class}, 'reset')
            call self._notifier[type].reset(a:loclist)
        endif
    endfor
endfunction

" Private methods {{{1

function! g:SyntasticNotifiers._initNotifiers()
    let self._notifier = {}
    for type in s:notifier_types
        let class = substitute(type, '\m.*', 'Syntastic\u&Notifier', '')
        let self._notifier[type] = g:{class}.New()
    endfor

    let self._enabled_types = copy(s:notifier_types)
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
