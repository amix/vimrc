if exists("g:loaded_syntastic_log_autoload") || !exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_log_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

let s:one_time_notices_issued = []

" Public functions {{{1

function! syntastic#log#info(msg) " {{{2
    echomsg "syntastic: info: " . a:msg
endfunction " }}}2

function! syntastic#log#warn(msg) " {{{2
    echohl WarningMsg
    echomsg "syntastic: warning: " . a:msg
    echohl None
endfunction " }}}2

function! syntastic#log#error(msg) " {{{2
    execute "normal \<Esc>"
    echohl ErrorMsg
    echomsg "syntastic: error: " . a:msg
    echohl None
endfunction " }}}2

function! syntastic#log#oneTimeWarn(msg) " {{{2
    if index(s:one_time_notices_issued, a:msg) >= 0
        return
    endif

    call add(s:one_time_notices_issued, a:msg)
    call syntastic#log#warn(a:msg)
endfunction " }}}2

" @vimlint(EVL102, 1, l:OLD_VAR)
function! syntastic#log#deprecationWarn(old, new, ...) " {{{2
    if exists('g:syntastic_' . a:old) && !exists('g:syntastic_' . a:new)
        let msg = 'variable g:syntastic_' . a:old . ' is deprecated, please use '

        if a:0
            let OLD_VAR = g:syntastic_{a:old}
            try
                let NEW_VAR = eval(a:1)
                let msg .= 'in its stead: let g:syntastic_' . a:new . ' = ' . string(NEW_VAR)
                let g:syntastic_{a:new} = NEW_VAR
            catch
                let msg .= 'g:syntastic_' . a:new . ' instead'
            endtry
        else
            let msg .= 'g:syntastic_' . a:new . ' instead'
            let g:syntastic_{a:new} = g:syntastic_{a:old}
        endif

        call syntastic#log#oneTimeWarn(msg)
    endif
endfunction " }}}2
" @vimlint(EVL102, 0, l:OLD_VAR)

function! syntastic#log#debug(level, msg, ...) " {{{2
    if !s:isDebugEnabled(a:level)
        return
    endif

    let leader = s:logTimestamp()
    call s:logRedirect(1)

    if a:0 > 0
        " filter out dictionary functions
        echomsg leader . a:msg . ' ' .
            \ strtrans(string(type(a:1) == type({}) || type(a:1) == type([]) ?
            \ filter(copy(a:1), 'type(v:val) != type(function("tr"))') : a:1))
    else
        echomsg leader . a:msg
    endif

    call s:logRedirect(0)
endfunction " }}}2

function! syntastic#log#debugShowOptions(level, names) " {{{2
    if !s:isDebugEnabled(a:level)
        return
    endif

    let leader = s:logTimestamp()
    call s:logRedirect(1)

    let vlist = copy(type(a:names) == type("") ? [a:names] : a:names)
    if !empty(vlist)
        call map(vlist, "'&' . v:val . ' = ' . strtrans(string(eval('&' . v:val)))")
        echomsg leader . join(vlist, ', ')
    endif
    call s:logRedirect(0)
endfunction " }}}2

function! syntastic#log#debugShowVariables(level, names) " {{{2
    if !s:isDebugEnabled(a:level)
        return
    endif

    let leader = s:logTimestamp()
    call s:logRedirect(1)

    let vlist = type(a:names) == type("") ? [a:names] : a:names
    for name in vlist
        let msg = s:formatVariable(name)
        if msg != ''
            echomsg leader . msg
        endif
    endfor

    call s:logRedirect(0)
endfunction " }}}2

function! syntastic#log#debugDump(level) " {{{2
    if !s:isDebugEnabled(a:level)
        return
    endif

    call syntastic#log#debugShowVariables( a:level, sort(keys(g:syntastic_defaults)) )
endfunction " }}}2

" }}}1

" Private functions {{{1

function! s:isDebugEnabled_smart(level) " {{{2
    return and(g:syntastic_debug, a:level)
endfunction " }}}2

function! s:isDebugEnabled_dumb(level) " {{{2
    " poor man's bit test for bit N, assuming a:level == 2**N
    return (g:syntastic_debug / a:level) % 2
endfunction " }}}2

let s:isDebugEnabled = function(exists('*and') ? 's:isDebugEnabled_smart' : 's:isDebugEnabled_dumb')
lockvar s:isDebugEnabled

function! s:logRedirect(on) " {{{2
    if exists("g:syntastic_debug_file")
        if a:on
            try
                execute 'redir >> ' . fnameescape(expand(g:syntastic_debug_file))
            catch /\m^Vim\%((\a\+)\)\=:/
                silent! redir END
                unlet g:syntastic_debug_file
            endtry
        else
            silent! redir END
        endif
    endif
endfunction " }}}2

function! s:logTimestamp() " {{{2
    return 'syntastic: ' . split(reltimestr(reltime(g:syntastic_start)))[0] . ': '
endfunction " }}}2

function! s:formatVariable(name) " {{{2
    let vals = []
    if exists('g:syntastic_' . a:name)
        call add(vals, 'g:syntastic_' . a:name . ' = ' . strtrans(string(g:syntastic_{a:name})))
    endif
    if exists('b:syntastic_' . a:name)
        call add(vals, 'b:syntastic_' . a:name . ' = ' . strtrans(string(b:syntastic_{a:name})))
    endif

    return join(vals, ', ')
endfunction " }}}2

" }}}1

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
