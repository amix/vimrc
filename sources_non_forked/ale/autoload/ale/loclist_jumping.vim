" Author: w0rp <devw0rp@gmail.com>
" Description: This file implements functions for jumping around in a file
"   based on ALE's internal loclist.

" Search for the nearest line either before or after the current position
" in the loclist. The argument 'wrap' can be passed to enable wrapping
" around the end of the list.
"
" If there are no items or we have hit the end with wrapping off, an empty
" List will be returned, otherwise a pair of [line_number, column_number] will
" be returned.
function! ale#loclist_jumping#FindNearest(direction, wrap, ...) abort
    let l:buffer = bufnr('')
    let l:pos = getpos('.')
    let l:info = get(g:ale_buffer_info, bufnr('%'), {'loclist': []})
    " Copy the list and filter to only the items in this buffer.
    let l:loclist = filter(copy(l:info.loclist), 'v:val.bufnr == l:buffer')
    let l:search_item = {'bufnr': l:buffer, 'lnum': l:pos[1], 'col': l:pos[2]}

    if a:0 > 0
        let l:filter = a:1
    else
        let l:filter = 'any'
    endif

    if a:0 > 1
        let l:subtype_filter = a:2
    else
        let l:subtype_filter = 'any'
    endif

    " When searching backwards, so we can find the next smallest match.
    if a:direction is# 'before'
        call reverse(l:loclist)
    endif

    " Look for items before or after the current position.
    for l:item in l:loclist
        " Compare the cursor with a item where the column number is bounded,
        " such that it's possible for the cursor to actually be on the given
        " column number, without modifying the cursor number we return. This
        " will allow us to move through matches, but still let us move the
        " cursor to a line without changing the column, in some cases.
        let l:cmp_value = ale#util#LocItemCompare(
        \   {
        \       'bufnr': l:buffer,
        \       'lnum': l:item.lnum,
        \       'col': min([
        \           max([l:item.col, 1]),
        \           max([len(getline(l:item.lnum)), 1]),
        \       ]),
        \   },
        \   l:search_item
        \)

        if (l:filter is# 'any' || l:filter is# l:item.type)
        \&& (
        \   l:subtype_filter is# 'any'
        \   || l:subtype_filter is# get(l:item, 'sub_type', '')
        \)

            if a:direction is# 'before' && l:cmp_value < 0
                return [l:item.lnum, l:item.col]
            endif

            if a:direction is# 'after' && l:cmp_value > 0
                return [l:item.lnum, l:item.col]
            endif
        endif
    endfor

    " If we found nothing, and the wrap option is set to 1, then we should
    " wrap around the list of warnings/errors
    if a:wrap
        for l:item in l:loclist
            if (l:filter is# 'any' || l:filter is# l:item.type)
            \&& (
            \   l:subtype_filter is# 'any'
            \   || l:subtype_filter is# get(l:item, 'sub_type', '')
            \)
                return [l:item.lnum, l:item.col]
            endif
        endfor
    endif

    return []
endfunction

" As before, find the nearest match, but position the cursor at it.
function! ale#loclist_jumping#Jump(direction, ...) abort
    if a:0 > 0
        let l:wrap = a:1
    else
        let l:wrap = 0
    endif

    if a:0 > 1
        let l:filter = a:2
    else
        let l:filter = 'any'
    endif

    if a:0 > 2
        let l:subtype_filter = a:3
    else
        let l:subtype_filter = 'any'
    endif

    let l:nearest = ale#loclist_jumping#FindNearest(a:direction,
    \   l:wrap, l:filter, l:subtype_filter)

    if !empty(l:nearest)
        normal! m`
        call cursor(l:nearest)
    endif
endfunction

function! ale#loclist_jumping#WrapJump(direction, sargs) abort
    let [l:args, l:rest] = ale#args#Parse(['error', 'warning', 'info', 'wrap',
    \                                      'style', 'nostyle'], a:sargs)

    let l:wrap = 0
    let l:type_filter = 'any'
    let l:subtype_filter = 'any'

    if get(l:args, 'wrap', 'nil') is# ''
        let l:wrap = 1
    endif

    if get(l:args, 'error', 'nil') is# ''
        let l:type_filter = 'E'
    elseif get(l:args, 'warning', 'nil') is# ''
        let l:type_filter = 'W'
    elseif get(l:args, 'info', 'nil') is# ''
        let l:type_filter = 'I'
    endif

    if get(l:args, 'nostyle', 'nil') is# ''
        let l:subtype_filter = 'style'
    elseif get(l:args, 'style', 'nil') is# ''
        let l:subtype_filter = ''
    endif

    call ale#loclist_jumping#Jump(a:direction, l:wrap, l:type_filter,
    \                             l:subtype_filter)
endfunction

function! ale#loclist_jumping#JumpToIndex(index) abort
    let l:buffer = bufnr('')
    let l:info = get(g:ale_buffer_info, l:buffer, {'loclist': []})
    let l:loclist = filter(copy(l:info.loclist), 'v:val.bufnr == l:buffer')

    if empty(l:loclist)
        return
    endif

    let l:item = l:loclist[a:index]

    if !empty(l:item)
        normal! m`
        call cursor([l:item.lnum, l:item.col])
    endif
endfunction
