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

let s:has_nvim_highlight = exists('*nvim_buf_add_highlight') && exists('*nvim_buf_clear_namespace')

if s:has_nvim_highlight
    let s:ns_id = nvim_create_namespace('ale_highlight')
endif

" Wrappers are necessary to test this functionality by faking the calls in tests.
function! ale#highlight#nvim_buf_add_highlight(buffer, ns_id, hl_group, line, col_start, col_end) abort
    " Ignore all errors for adding highlights.
    try
        call nvim_buf_add_highlight(a:buffer, a:ns_id, a:hl_group, a:line, a:col_start, a:col_end)
    catch
    endtry
endfunction

function! ale#highlight#nvim_buf_clear_namespace(buffer, ns_id, line_start, line_end) abort
    call nvim_buf_clear_namespace(a:buffer, a:ns_id, a:line_start, a:line_end)
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
    if s:has_nvim_highlight
        call ale#highlight#nvim_buf_clear_namespace(bufnr(''), s:ns_id, 0, -1)
    else
        for l:match in getmatches()
            if l:match.group =~? '\v^ALE(Style)?(Error|Warning|Info)(Line)?$'
                call matchdelete(l:match.id)
            endif
        endfor
    endif
endfunction

" Same semantics of matchaddpos but will use nvim_buf_add_highlight if
" available. This involves iterating over the position list, switching from
" 1-based indexing to 0-based indexing, and translating the multiple ways
" that position can be specified for matchaddpos into line + col_start +
" col_end.
function! s:matchaddpos(group, pos_list) abort
    if s:has_nvim_highlight
        for l:pos in a:pos_list
            let l:line = type(l:pos) == v:t_number
            \   ? l:pos - 1
            \   : l:pos[0] - 1

            if type(l:pos) == v:t_number || len(l:pos) == 1
                let l:col_start = 0
                let l:col_end = s:MAX_COL_SIZE
            else
                let l:col_start = l:pos[1] - 1
                let l:col_end = l:col_start + get(l:pos, 2, 1)
            endif

            call ale#highlight#nvim_buf_add_highlight(
            \   bufnr(''),
            \   s:ns_id,
            \   a:group,
            \   l:line,
            \   l:col_start,
            \   l:col_end,
            \)
        endfor
    else
        call matchaddpos(a:group, a:pos_list)
    endif
endfunction

function! s:highlight_line(bufnr, lnum, group) abort
    call s:matchaddpos(a:group, [a:lnum])
endfunction

function! s:highlight_range(bufnr, range, group) abort
    " Set all of the positions, which are chunked into Lists which
    " are as large as will be accepted by matchaddpos.
    call map(
    \   ale#highlight#CreatePositions(
    \       a:range.lnum,
    \       a:range.col,
    \       a:range.end_lnum,
    \       a:range.end_col
    \   ),
    \   's:matchaddpos(a:group, v:val)'
    \)
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

    let l:exclude_list = ale#Var(a:buffer, 'exclude_highlights')

    if !empty(l:exclude_list)
        call filter(l:new_list, 'empty(ale#util#GetMatches(v:val.text, l:exclude_list))')
    endif

    " Update highlights for the current buffer, which may or may not
    " be the buffer we just set highlights for.
    call ale#highlight#UpdateHighlights()
endfunction
