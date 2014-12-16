"CLASS: Notifier
"============================================================
let s:Notifier = {}

function! s:Notifier.AddListener(event, funcname)
    let listeners = s:Notifier.GetListenersForEvent(a:event)
    if listeners == []
        let listenersMap = s:Notifier.GetListenersMap()
        let listenersMap[a:event] = listeners
    endif
    call add(listeners, a:funcname)
endfunction

function! s:Notifier.NotifyListeners(event, path, params)
    let event = g:NERDTreeEvent.New(b:NERDTree, a:path, a:event, a:params)

    for listener in s:Notifier.GetListenersForEvent(a:event)
        call {listener}(event)
    endfor
endfunction

function! s:Notifier.GetListenersMap()
    if !exists("s:refreshListenersMap")
        let s:refreshListenersMap = {}
    endif
    return s:refreshListenersMap
endfunction

function! s:Notifier.GetListenersForEvent(name)
    let listenersMap = s:Notifier.GetListenersMap()
    return get(listenersMap, a:name, [])
endfunction

let g:NERDTreePathNotifier = deepcopy(s:Notifier)

