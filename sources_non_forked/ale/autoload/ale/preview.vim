" Author: w0rp <devw0rp@gmail.com>
" Description: Preview windows for showing whatever information in.

" Open a preview window and show some lines in it.
" An optional second argument can set an alternative filetype for the window.
function! ale#preview#Show(lines, ...) abort
    let l:filetype = get(a:000, 0, 'ale-preview')

    silent pedit ALEPreviewWindow
    wincmd P
    setlocal modifiable
    setlocal noreadonly
    setlocal nobuflisted
    let &l:filetype = l:filetype
    setlocal buftype=nofile
    setlocal bufhidden=wipe
    :%d
    call setline(1, a:lines)
    setlocal nomodifiable
    setlocal readonly
endfunction

" Show a location selection preview window, given some items.
" Each item should have 'filename', 'line', and 'column' keys.
function! ale#preview#ShowSelection(item_list) abort
    let l:lines = []

    " Create lines to display to users.
    for l:item in a:item_list
        call add(
        \   l:lines,
        \   l:item.filename
        \       . ':' . l:item.line
        \       . ':' . l:item.column,
        \)
    endfor

    call ale#preview#Show(l:lines, 'ale-preview-selection')
    let b:ale_preview_item_list = a:item_list
endfunction

function! s:Open(open_in_tab) abort
    let l:item_list = get(b:, 'ale_preview_item_list', [])
    let l:item = get(l:item_list, getcurpos()[1] - 1, {})

    if empty(l:item)
        return
    endif

    if !a:open_in_tab
        :q!
    endif

    call ale#util#Open(
    \   l:item.filename,
    \   l:item.line,
    \   l:item.column,
    \   {'open_in_tab': a:open_in_tab},
    \)
endfunction

function! ale#preview#OpenSelectionInBuffer() abort
    call s:Open(0)
endfunction

function! ale#preview#OpenSelectionInTab() abort
    call s:Open(1)
endfunction
