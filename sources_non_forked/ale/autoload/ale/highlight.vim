scriptencoding utf8
" Author: w0rp <devw0rp@gmail.com>
" Description: This module implements error/warning highlighting.

if !hlexists('ALEError')
    highlight link ALEError SpellBad
endif

if !hlexists('ALEStyleError')
    highlight link ALEStyleError ALEError
endif

if !hlexists('ALEWarning')
    highlight link ALEWarning SpellCap
endif

if !hlexists('ALEStyleWarning')
    highlight link ALEStyleWarning ALEWarning
endif

if !hlexists('ALEInfo')
    highlight link ALEInfo ALEWarning
endif

" The maximum number of items for the second argument of matchaddpos()
let s:MAX_POS_VALUES = 8
let s:MAX_COL_SIZE = 1073741824 " pow(2, 30)

" Check if we have neovim's buffer highlight API
"
" Below we define some functions' implementation conditionally if this API
" exists or not.
"
" The API itself is more ergonomic and neovim performs highlights positions
" rebases during edits so we see less stalled highlights.
let s:nvim_api = exists('*nvim_buf_add_highlight') && exists('*nvim_buf_clear_namespace')

function! ale#highlight#HasNeovimApi() abort
    return s:nvim_api
endfunction

function! ale#highlight#nvim_buf_clear_namespace(...) abort
    return call('nvim_buf_clear_namespace', a:000)
endfunction

function! ale#highlight#nvim_buf_add_highlight(...) abort
    return call('nvim_buf_add_highlight', a:000)
endfunction

function! s:ale_nvim_highlight_id(bufnr) abort
    let l:id = getbufvar(a:bufnr, 'ale_nvim_highlight_id', -1)

    if l:id is -1
        " NOTE: This will highlight nothing but will allocate new id
        let l:id = ale#highlight#nvim_buf_add_highlight(
        \   a:bufnr, 0, '', 0, 0, -1
        \)
        call setbufvar(a:bufnr, 'ale_nvim_highlight_id', l:id)
    endif

    return l:id
endfunction

function! ale#highlight#CreatePositions(line, col, end_line, end_col) abort
    if a:line >= a:end_line
        " For single lines, just return the one position.
        return [[[a:line, a:col, a:end_col - a:col + 1]]]
    endif

    " Get positions from the first line at the first column, up to a large
    " integer for highlighting up to the end of the line, followed by
    " the lines in-between, for highlighting entire lines, and
    " a highlight for the last line, up to the end column.
    let l:all_positions =
    \   [[a:line, a:col, s:MAX_COL_SIZE]]
    \   + range(a:line + 1, a:end_line - 1)
    \   + [[a:end_line, 1, a:end_col]]

    return map(
    \   range(0, len(l:all_positions) - 1, s:MAX_POS_VALUES),
    \   'l:all_positions[v:val : v:val + s:MAX_POS_VALUES - 1]',
    \)
endfunction

" Given a loclist for current items to highlight, remove all highlights
" except these which have matching loclist item entries.

function! ale#highlight#RemoveHighlights() abort
    if ale#highlight#HasNeovimApi()
        if get(b:, 'ale_nvim_highlight_id', 0)
            let l:bufnr = bufnr('%')
            " NOTE: 0, -1 means from 0 line till the end of buffer
            call ale#highlight#nvim_buf_clear_namespace(
            \   l:bufnr,
            \   b:ale_nvim_highlight_id,
            \   0, -1
            \)
        endif
    else
        for l:match in getmatches()
            if l:match.group =~# '^ALE'
                call matchdelete(l:match.id)
            endif
        endfor
    endif
endfunction

function! s:highlight_line(bufnr, lnum, group) abort
    if ale#highlight#HasNeovimApi()
        let l:highlight_id = s:ale_nvim_highlight_id(a:bufnr)
        call ale#highlight#nvim_buf_add_highlight(
        \   a:bufnr, l:highlight_id, a:group,
        \   a:lnum - 1, 0, -1
        \)
    else
        call matchaddpos(a:group, [a:lnum])
    endif
endfunction

function! s:highlight_range(bufnr, range, group) abort
    if ale#highlight#HasNeovimApi()
        let l:highlight_id = s:ale_nvim_highlight_id(a:bufnr)
        " NOTE: lines and columns indicies are 0-based in nvim_buf_* API.
        let l:lnum = a:range.lnum - 1
        let l:end_lnum = a:range.end_lnum - 1
        let l:col = a:range.col - 1
        let l:end_col = a:range.end_col

        if l:lnum >= l:end_lnum
            " For single lines, just return the one position.
            call ale#highlight#nvim_buf_add_highlight(
            \   a:bufnr, l:highlight_id, a:group,
            \   l:lnum, l:col, l:end_col
            \)
        else
            " highlight first line from start till the line end
            call ale#highlight#nvim_buf_add_highlight(
            \   a:bufnr, l:highlight_id, a:group,
            \   l:lnum, l:col, -1
            \)

            " highlight all lines between the first and last entirely
            let l:cur = l:lnum + 1

            while l:cur < l:end_lnum
                call ale#highlight#nvim_buf_add_highlight(
                \   a:bufnr, l:highlight_id, a:group,
                \   l:cur, 0, -1
                \   )
                let l:cur += 1
            endwhile

            call ale#highlight#nvim_buf_add_highlight(
            \   a:bufnr, l:highlight_id, a:group,
            \   l:end_lnum, 0, l:end_col
            \)
        endif
    else
        " Set all of the positions, which are chunked into Lists which
        " are as large as will be accepted by matchaddpos.
        call map(
        \   ale#highlight#CreatePositions(
        \       a:range.lnum,
        \       a:range.col,
        \       a:range.end_lnum,
        \       a:range.end_col
        \   ),
        \   'matchaddpos(a:group, v:val)'
        \)
    endif
endfunction

function! ale#highlight#UpdateHighlights() abort
    let l:item_list = get(b:, 'ale_enabled', 1) && g:ale_enabled
    \   ? get(b:, 'ale_highlight_items', [])
    \   : []

    call ale#highlight#RemoveHighlights()

    for l:item in l:item_list
        if l:item.type is# 'W'
            if get(l:item, 'sub_type', '') is# 'style'
                let l:group = 'ALEStyleWarning'
            else
                let l:group = 'ALEWarning'
            endif
        elseif l:item.type is# 'I'
            let l:group = 'ALEInfo'
        elseif get(l:item, 'sub_type', '') is# 'style'
            let l:group = 'ALEStyleError'
        else
            let l:group = 'ALEError'
        endif

        let l:range = {
        \   'lnum': l:item.lnum,
        \   'col': l:item.col,
        \   'end_lnum': get(l:item, 'end_lnum', l:item.lnum),
        \   'end_col': get(l:item, 'end_col', l:item.col)
        \}

        call s:highlight_range(l:item.bufnr, l:range, l:group)
    endfor

    " If highlights are enabled and signs are not enabled, we should still
    " offer line highlights by adding a separate set of highlights.
    if !g:ale_set_signs
        let l:available_groups = {
        \   'ALEWarningLine': hlexists('ALEWarningLine'),
        \   'ALEInfoLine': hlexists('ALEInfoLine'),
        \   'ALEErrorLine': hlexists('ALEErrorLine'),
        \}

        for l:item in l:item_list
            if l:item.type is# 'W'
                let l:group = 'ALEWarningLine'
            elseif l:item.type is# 'I'
                let l:group = 'ALEInfoLine'
            else
                let l:group = 'ALEErrorLine'
            endif

            if l:available_groups[l:group]
                call s:highlight_line(l:item.bufnr, l:item.lnum, l:group)
            endif
        endfor
    endif
endfunction

function! ale#highlight#BufferHidden(buffer) abort
    " Remove highlights right away when buffers are hidden.
    " They will be restored later when buffers are entered.
    call ale#highlight#RemoveHighlights()
endfunction

augroup ALEHighlightBufferGroup
    autocmd!
    autocmd BufEnter * call ale#highlight#UpdateHighlights()
    autocmd BufHidden * call ale#highlight#BufferHidden(expand('<abuf>'))
augroup END

function! ale#highlight#SetHighlights(buffer, loclist) abort
    let l:new_list = getbufvar(a:buffer, 'ale_enabled', 1) && g:ale_enabled
    \   ? filter(copy(a:loclist), 'v:val.bufnr == a:buffer && v:val.col > 0')
    \   : []

    " Set the list in the buffer variable.
    call setbufvar(str2nr(a:buffer), 'ale_highlight_items', l:new_list)

    " Update highlights for the current buffer, which may or may not
    " be the buffer we just set highlights for.
    call ale#highlight#UpdateHighlights()
endfunction
