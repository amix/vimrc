if exists("g:loaded_syntastic_log_autoload")
    finish
endif
let g:loaded_syntastic_log_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

if !exists("g:syntastic_debug")
    let g:syntastic_debug = 0
endif

let s:global_options = [
    \ 'syntastic_aggregate_errors',
    \ 'syntastic_always_populate_loc_list',
    \ 'syntastic_auto_jump',
    \ 'syntastic_auto_loc_list',
    \ 'syntastic_check_on_open',
    \ 'syntastic_check_on_wq',
    \ 'syntastic_debug',
    \ 'syntastic_echo_current_error',
    \ 'syntastic_enable_balloons',
    \ 'syntastic_enable_highlighting',
    \ 'syntastic_enable_signs',
    \ 'syntastic_error_symbol',
    \ 'syntastic_filetype_map',
    \ 'syntastic_full_redraws',
    \ 'syntastic_id_checkers',
    \ 'syntastic_ignore_files',
    \ 'syntastic_loc_list_height',
    \ 'syntastic_mode_map',
    \ 'syntastic_quiet_messages',
    \ 'syntastic_reuse_loc_lists',
    \ 'syntastic_stl_format',
    \ 'syntastic_style_error_symbol',
    \ 'syntastic_style_warning_symbol',
    \ 'syntastic_warning_symbol' ]

let s:deprecation_notices_issued = []

" Public functions {{{1

function! syntastic#log#info(msg)
    echomsg "syntastic: info: " . a:msg
endfunction

function! syntastic#log#warn(msg)
    echohl WarningMsg
    echomsg "syntastic: warning: " . a:msg
    echohl None
endfunction

function! syntastic#log#error(msg)
    execute "normal \<Esc>"
    echohl ErrorMsg
    echomsg "syntastic: error: " . a:msg
    echohl None
endfunction

function! syntastic#log#deprecationWarn(msg)
    if index(s:deprecation_notices_issued, a:msg) >= 0
        return
    endif

    call add(s:deprecation_notices_issued, a:msg)
    call syntastic#log#warn(a:msg)
endfunction

function! syntastic#log#debug(level, msg, ...)
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
endfunction

function! syntastic#log#debugShowOptions(level, names)
    if !s:isDebugEnabled(a:level)
        return
    endif

    let leader = s:logTimestamp()
    call s:logRedirect(1)

    let vlist = type(a:names) == type("") ? [a:names] : a:names
    if !empty(vlist)
        call map(vlist, "'&' . v:val . ' = ' . strtrans(string(eval('&' . v:val)))")
        echomsg leader . join(vlist, ', ')
    endif
    call s:logRedirect(0)
endfunction

function! syntastic#log#debugShowVariables(level, names)
    if !s:isDebugEnabled(a:level)
        return
    endif

    let leader = s:logTimestamp()
    call s:logRedirect(1)

    let vlist = type(a:names) == type("") ? [a:names] : a:names
    for name in vlist
        echomsg leader . s:formatVariable(name)
    endfor

    call s:logRedirect(0)
endfunction

function! syntastic#log#debugDump(level)
    if !s:isDebugEnabled(a:level)
        return
    endif

    call syntastic#log#debugShowVariables(a:level, s:global_options)
endfunction

" Private functions {{{1

function! s:isDebugEnabled_smart(level)
    return and(g:syntastic_debug, a:level)
endfunction

function! s:isDebugEnabled_dumb(level)
    " poor man's bit test for bit N, assuming a:level == 2**N
    return (g:syntastic_debug / a:level) % 2
endfunction

let s:isDebugEnabled = function(exists('*and') ? 's:isDebugEnabled_smart' : 's:isDebugEnabled_dumb')

function! s:logRedirect(on)
    if exists("g:syntastic_debug_file")
        if a:on
            try
                execute 'redir >> ' . fnameescape(expand(g:syntastic_debug_file))
            catch /^Vim\%((\a\+)\)\=:/
                silent! redir END
                unlet g:syntastic_debug_file
            endtry
        else
            silent! redir END
        endif
    endif
endfunction

function! s:logTimestamp_smart()
    return 'syntastic: ' . split(reltimestr(reltime(g:syntastic_start)))[0] . ': '
endfunction

function! s:logTimestamp_dumb()
    return 'syntastic: debug: '
endfunction

let s:logTimestamp = function(has('reltime') ? 's:logTimestamp_smart' : 's:logTimestamp_dumb')

function! s:formatVariable(name)
    let vals = []
    if exists('g:' . a:name)
        call add(vals, 'g:' . a:name . ' = ' . strtrans(string(g:{a:name})))
    endif
    if exists('b:' . a:name)
        call add(vals, 'b:' . a:name . ' = ' . strtrans(string(b:{a:name})))
    endif

    return join(vals, ', ')
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
" vim: set et sts=4 sw=4 fdm=marker:
