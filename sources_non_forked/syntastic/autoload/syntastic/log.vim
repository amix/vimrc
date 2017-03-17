if exists('g:loaded_syntastic_log_autoload') || !exists('g:loaded_syntastic_plugin')
    finish
endif
let g:loaded_syntastic_log_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

let s:one_time_notices_issued = []

" Public functions {{{1

function! syntastic#log#info(msg) abort " {{{2
    echomsg 'syntastic: info: ' . a:msg
endfunction " }}}2

function! syntastic#log#warn(msg) abort " {{{2
    echohl WarningMsg
    echomsg 'syntastic: warning: ' . a:msg
    echohl None
endfunction " }}}2

function! syntastic#log#error(msg) abort " {{{2
    execute 'normal! \<Esc>'
    echohl ErrorMsg
    echomsg 'syntastic: error: ' . a:msg
    echohl None
endfunction " }}}2

function! syntastic#log#oneTimeWarn(msg) abort " {{{2
    if index(s:one_time_notices_issued, a:msg) >= 0
        return
    endif

    call add(s:one_time_notices_issued, a:msg)
    call syntastic#log#warn(a:msg)
endfunction " }}}2

" @vimlint(EVL102, 1, l:OLD_VAR)
function! syntastic#log#deprecationWarn(old, new, ...) abort " {{{2
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

function! syntastic#log#debug(level, msg, ...) abort " {{{2
    if !s:_isDebugEnabled(a:level)
        return
    endif

    let leader = s:_log_timestamp()
    call s:_logRedirect(1)

    if a:0
        " filter out dictionary functions
        echomsg leader . a:msg . ' ' .
            \ strtrans(string(type(a:1) == type({}) || type(a:1) == type([]) ?
            \ filter(copy(a:1), 'type(v:val) != type(function("tr"))') : a:1))
    else
        echomsg leader . a:msg
    endif

    call s:_logRedirect(0)
endfunction " }}}2

function! syntastic#log#debugShowOptions(level, names) abort " {{{2
    if !s:_isDebugEnabled(a:level)
        return
    endif

    let leader = s:_log_timestamp()
    call s:_logRedirect(1)

    let vlist = copy(type(a:names) == type('') ? [a:names] : a:names)
    let add_shell = index(vlist, 'shell') >= 0 && &shell !=# syntastic#util#var('shell')
    if !empty(vlist)
        call map(vlist, "'&' . v:val . ' = ' . strtrans(string(eval('&' . v:val))) . (s:_is_modified(v:val) ? ' (!)' : '')")
        if add_shell
            call add(vlist, 'u:shell = ' . strtrans(string(syntastic#util#var('shell'))) . ' (!)')
        endif
        echomsg leader . join(vlist, ', ')
    endif
    call s:_logRedirect(0)
endfunction " }}}2

function! syntastic#log#debugShowVariables(level, names) abort " {{{2
    if !s:_isDebugEnabled(a:level)
        return
    endif

    let leader = s:_log_timestamp()
    call s:_logRedirect(1)

    let vlist = type(a:names) == type('') ? [a:names] : a:names
    for name in vlist
        let msg = s:_format_variable(name)
        if msg !=# ''
            echomsg leader . msg
        endif
    endfor

    call s:_logRedirect(0)
endfunction " }}}2

function! syntastic#log#debugDump(level) abort " {{{2
    if !s:_isDebugEnabled(a:level)
        return
    endif

    call syntastic#log#debugShowVariables( a:level, sort(keys(g:_SYNTASTIC_DEFAULTS)) )
endfunction " }}}2

function! syntastic#log#ndebug(level, title, messages) abort " {{{2
    if s:_isDebugEnabled(a:level)
        return
    endif

    call syntastic#log#error(a:title)
    if type(a:messages) == type([])
        for msg in a:messages
            echomsg msg
        endfor
    else
        echomsg a:messages
    endif
endfunction " }}}2

" }}}1

" Private functions {{{1

function! s:_isDebugEnabled_smart(level) abort " {{{2
    return and(g:syntastic_debug, a:level)
endfunction " }}}2

function! s:_isDebugEnabled_dumb(level) abort " {{{2
    " poor man's bit test for bit N, assuming a:level == 2**N
    return (g:syntastic_debug / a:level) % 2
endfunction " }}}2

let s:_isDebugEnabled = function(exists('*and') ? 's:_isDebugEnabled_smart' : 's:_isDebugEnabled_dumb')
lockvar s:_isDebugEnabled

function! s:_logRedirect(on) abort " {{{2
    if exists('g:syntastic_debug_file')
        if a:on
            try
                execute 'redir >> ' . fnameescape(expand(g:syntastic_debug_file, 1))
            catch /\m^Vim\%((\a\+)\)\=:/
                silent! redir END
                unlet g:syntastic_debug_file
            endtry
        else
            silent! redir END
        endif
    endif
endfunction " }}}2

" }}}1

" Utilities {{{1

function! s:_log_timestamp_smart() abort " {{{2
    return printf('syntastic: %f: ', reltimefloat(reltime(g:_SYNTASTIC_START)))
endfunction " }}}2

function! s:_log_timestamp_dumb() abort " {{{2
    return 'syntastic: ' . split(reltimestr(reltime(g:_SYNTASTIC_START)))[0] . ': '
endfunction " }}}2

let s:_log_timestamp = function(has('float') && exists('*reltimefloat') ? 's:_log_timestamp_smart' : 's:_log_timestamp_dumb')
lockvar s:_log_timestamp

function! s:_format_variable(name) abort " {{{2
    let vals = []
    if exists('g:syntastic_' . a:name)
        call add(vals, 'g:syntastic_' . a:name . ' = ' . strtrans(string(g:syntastic_{a:name})))
    endif
    if exists('b:syntastic_' . a:name)
        call add(vals, 'b:syntastic_' . a:name . ' = ' . strtrans(string(b:syntastic_{a:name})))
    endif

    return join(vals, ', ')
endfunction " }}}2

function! s:_is_modified(name) abort " {{{2
    if !exists('s:option_defaults')
        let s:option_defaults = {}
    endif
    if !has_key(s:option_defaults, a:name)
        let opt_save = eval('&' . a:name)
        execute 'set ' . a:name . '&'
        let s:option_defaults[a:name] = eval('&' . a:name)
        execute 'let &' . a:name . ' = ' . string(opt_save)
    endif

    return s:option_defaults[a:name] !=# eval('&' . a:name)
endfunction " }}}2

" }}}1

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
