scriptencoding utf-8
" Author: w0rp <devw0rp@gmail.com>
" Author: Luan Santos <cfcluan@gmail.com>
" Description: Shows lint message for the current line as virtualtext, if any

if !hlexists('ALEVirtualTextError')
    highlight link ALEVirtualTextError Comment
endif

if !hlexists('ALEVirtualTextStyleError')
    highlight link ALEVirtualTextStyleError ALEVirtualTextError
endif

if !hlexists('ALEVirtualTextWarning')
    highlight link ALEVirtualTextWarning Comment
endif

if !hlexists('ALEVirtualTextStyleWarning')
    highlight link ALEVirtualTextStyleWarning ALEVirtualTextWarning
endif

if !hlexists('ALEVirtualTextInfo')
    highlight link ALEVirtualTextInfo ALEVirtualTextWarning
endif

let g:ale_virtualtext_prefix =
\   get(g:, 'ale_virtualtext_prefix', '%comment% %type%: ')
" Controls the milliseconds delay before showing a message.
let g:ale_virtualtext_delay = get(g:, 'ale_virtualtext_delay', 10)

" Controls the positioning of virtualtext
let g:ale_virtualtext_column = get(g:, 'ale_virtualtext_column', 0)
let g:ale_virtualtext_maxcolumn = get(g:, 'ale_virtualtext_maxcolumn', 0)
" If 1, only show the first problem with virtualtext.
let g:ale_virtualtext_single = get(g:, 'ale_virtualtext_single', 1)

let s:cursor_timer = get(s:, 'cursor_timer', -1)
let s:last_pos = get(s:, 'last_pos', [0, 0, 0])
let s:hl_list = get(s:, 'hl_list', [])
let s:last_message = ''

if !has_key(s:, 'has_virt_text')
    let s:has_virt_text = 0
    let s:emulate_virt = 0
    let s:last_virt = -1

    if has('nvim-0.3.2')
        let s:ns_id = nvim_create_namespace('ale')
        let s:has_virt_text = 1
    elseif has('textprop') && has('popupwin')
        let s:has_virt_text = 1
        let s:emulate_virt = !has('patch-9.0.0297')

        if s:emulate_virt
            call prop_type_add('ale', {})
        endif
    endif
endif

function! s:StopCursorTimer() abort
    if s:cursor_timer != -1
        call timer_stop(s:cursor_timer)
        let s:cursor_timer = -1
    endif
endfunction

function! ale#virtualtext#ResetDataForTests() abort
    let s:last_pos = [0, 0, 0]
    let s:last_message = ''
endfunction

function! ale#virtualtext#GetLastMessageForTests() abort
    return s:last_message
endfunction

function! ale#virtualtext#GetComment(buffer) abort
    let l:filetype = getbufvar(a:buffer, '&filetype')
    let l:split = split(getbufvar(a:buffer, '&commentstring'), '%s')

    return !empty(l:split) ? trim(l:split[0]) : '#'
endfunction

function! ale#virtualtext#Clear(buffer) abort
    if !s:has_virt_text || !bufexists(str2nr(a:buffer))
        return
    endif

    if has('nvim')
        call nvim_buf_clear_namespace(a:buffer, s:ns_id, 0, -1)
    else
        if s:emulate_virt && s:last_virt != -1
            call prop_remove({'type': 'ale'})
            call popup_close(s:last_virt)
            let s:last_virt = -1
        elseif !empty(s:hl_list)
            call prop_remove({
            \   'types': s:hl_list,
            \   'all': 1,
            \   'bufnr': a:buffer,
            \})
        endif
    endif
endfunction

function! ale#virtualtext#GetGroup(item) abort
    let l:type = get(a:item, 'type', 'E')
    let l:sub_type = get(a:item, 'sub_type', '')

    if l:type is# 'E'
        if l:sub_type is# 'style'
            return 'ALEVirtualTextStyleError'
        endif

        return 'ALEVirtualTextError'
    endif

    if l:type is# 'W'
        if l:sub_type is# 'style'
            return 'ALEVirtualTextStyleWarning'
        endif

        return 'ALEVirtualTextWarning'
    endif

    return 'ALEVirtualTextInfo'
endfunction

function! ale#virtualtext#GetColumnPadding(buffer, line) abort
    let l:mincol = ale#Var(a:buffer, 'virtualtext_column')
    let l:maxcol = ale#Var(a:buffer, 'virtualtext_maxcolumn')
    let l:win = bufwinnr(a:buffer)

    if l:mincol[len(l:mincol)-1] is# '%'
        let l:mincol = (winwidth(l:win) * l:mincol) / 100
    endif

    if l:maxcol[len(l:maxcol)-1] is# '%'
        let l:maxcol = (winwidth(l:win) * l:maxcol) / 100
    endif

    " Calculate padding for virtualtext alignment
    if l:mincol > 0 || l:maxcol > 0
        let l:line_width = strdisplaywidth(getline(a:line))

        if l:line_width < l:mincol
            return l:mincol - l:line_width
        elseif l:maxcol > 0 && l:line_width >= l:maxcol
            " Stop processing if virtualtext would start beyond maxcol
            return -1
        endif
    endif

    " no padding.
    return 0
endfunction

function! ale#virtualtext#ShowMessage(buffer, item) abort
    if !s:has_virt_text || !bufexists(str2nr(a:buffer))
        return
    endif

    let l:line = max([1, a:item.lnum])
    let l:hl_group = ale#virtualtext#GetGroup(a:item)

    " Get a language-appropriate comment character, or default to '#'.
    let l:comment = ale#virtualtext#GetComment(a:buffer)
    let l:prefix = ale#Var(a:buffer, 'virtualtext_prefix')
    let l:prefix = ale#GetLocItemMessage(a:item, l:prefix)
    let l:prefix = substitute(l:prefix, '\V%comment%', '\=l:comment', 'g')
    let l:msg = l:prefix . substitute(a:item.text, '\n', ' ', 'g')
    let l:col_pad = ale#virtualtext#GetColumnPadding(a:buffer, l:line)

    " Store the last message we're going to set so we can read it in tests.
    let s:last_message = l:msg

    " Discard virtualtext if padding is negative.
    if l:col_pad < 0
        return
    endif

    if has('nvim')
        call nvim_buf_set_virtual_text(
        \   a:buffer,
        \   s:ns_id, l:line - 1,
        \   [[l:msg, l:hl_group]],
        \   {}
        \)
    elseif s:emulate_virt
        let l:left_pad = col('$')
        call prop_add(l:line, l:left_pad, {'type': 'ale'})
        let s:last_virt = popup_create(l:msg, {
        \   'line': -1,
        \   'padding': [0, 0, 0, 1],
        \   'mask': [[1, 1, 1, 1]],
        \   'textprop': 'ale',
        \   'highlight': l:hl_group,
        \   'fixed': 1,
        \   'wrap': 0,
        \   'zindex': 2
        \})
    else
        let l:type = prop_type_get(l:hl_group)

        if l:type == {}
            call prop_type_add(l:hl_group, {'highlight': l:hl_group})
        endif

        " Add highlight groups to the list so we can clear them later.
        if index(s:hl_list, l:hl_group) == -1
            call add(s:hl_list, l:hl_group)
        endif

        " We ignore all errors from prop_add.
        silent! call prop_add(l:line, 0, {
        \   'type': l:hl_group,
        \   'text': ' ' . l:msg,
        \   'bufnr': a:buffer,
        \   'text_padding_left': l:col_pad,
        \})
    endif
endfunction

function! ale#virtualtext#ShowCursorWarning(...) abort
    if g:ale_virtualtext_cursor isnot# 'current'
    \&& g:ale_virtualtext_cursor != 1
        return
    endif

    let l:buffer = bufnr('')

    if mode(1) isnot# 'n'
    \|| g:ale_use_neovim_diagnostics_api
    \|| ale#ShouldDoNothing(l:buffer)
        return
    endif

    let [l:info, l:item] = ale#util#FindItemAtCursor(l:buffer)
    call ale#virtualtext#Clear(l:buffer)

    if !empty(l:item)
        call ale#virtualtext#ShowMessage(l:buffer, l:item)
    endif
endfunction

function! ale#virtualtext#ShowCursorWarningWithDelay() abort
    let l:buffer = bufnr('')

    if g:ale_virtualtext_cursor isnot# 'current'
    \&& g:ale_virtualtext_cursor != 1
        return
    endif

    call s:StopCursorTimer()

    if mode(1) isnot# 'n'
    \|| g:ale_use_neovim_diagnostics_api
        return
    endif

    let l:pos = getpos('.')[0:2]

    " Check the current buffer, line, and column number against the last
    " recorded position. If the position has actually changed, *then*
    " we should show something. Otherwise we can end up doing processing
    " the show message far too frequently.
    if l:pos != s:last_pos
        let l:delay = ale#Var(l:buffer, 'virtualtext_delay')

        let s:last_pos = l:pos
        let s:cursor_timer = timer_start(
        \   l:delay,
        \   function('ale#virtualtext#ShowCursorWarning')
        \)
    endif
endfunction

function! ale#virtualtext#CompareSeverityPerLine(left, right) abort
    " Compare lines
    if a:left.lnum < a:right.lnum
        return -1
    endif

    if a:left.lnum > a:right.lnum
        return 1
    endif

    let l:left_priority = ale#util#GetItemPriority(a:left)
    let l:right_priority = ale#util#GetItemPriority(a:right)

    " Put highest priority items first.
    if l:left_priority > l:right_priority
        return -1
    endif

    if l:left_priority < l:right_priority
        return 1
    endif

    " Put the first seen problem first.
    return a:left.col - a:right.col
endfunction

function! ale#virtualtext#SetTexts(buffer, loclist) abort
    if !has('nvim') && s:emulate_virt
        return
    endif

    call ale#virtualtext#Clear(a:buffer)

    let l:buffer_list = filter(copy(a:loclist), 'v:val.bufnr == a:buffer')

    if ale#Var(a:buffer,'virtualtext_single')
        " If we want a single problem per line, sort items on each line by
        " highest severity and then lowest column position, then de-duplicate
        " the items by line.
        call uniq(
        \   sort(l:buffer_list, function('ale#virtualtext#CompareSeverityPerLine')),
        \   {a, b -> a.lnum - b.lnum}
        \)
    endif

    for l:item in l:buffer_list
        call ale#virtualtext#ShowMessage(a:buffer, l:item)
    endfor
endfunction
