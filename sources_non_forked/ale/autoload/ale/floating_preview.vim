" Author: Jan-Grimo Sobez <jan-grimo.sobez@phys.chem.ethz.ch>
" Author: Kevin Clark <kevin.clark@gmail.com>
" Description: Floating preview window for showing whatever information in.

" Precondition: exists('*nvim_open_win')

function! ale#floating_preview#Show(lines, ...) abort
    if !exists('*nvim_open_win')
        execute 'echom ''Floating windows not supported in this vim instance.'''

        return
    endif

    " Remove the close autocmd so it doesn't happen mid update
    augroup ale_floating_preview_window
        autocmd!
    augroup END

    let l:options = get(a:000, 0, {})

    " Only create a new window if we need it
    if !exists('w:preview') || index(nvim_list_wins(), w:preview['id']) is# -1
        call s:Create(l:options)
    else
        call nvim_buf_set_option(w:preview['buffer'], 'modifiable', v:true)
    endif

    " Execute commands in window context
    let l:parent_window = nvim_get_current_win()

    call nvim_set_current_win(w:preview['id'])

    for l:command in get(l:options, 'commands', [])
        call execute(l:command)
    endfor

    call nvim_set_current_win(l:parent_window)

    " Return to parent context on move
    augroup ale_floating_preview_window
        autocmd!

        if g:ale_close_preview_on_insert
            autocmd CursorMoved,TabLeave,WinLeave,InsertEnter <buffer> ++once call s:Close()
        else
            autocmd CursorMoved,TabLeave,WinLeave <buffer> ++once call s:Close()
        endif
    augroup END

    let l:width = max(map(copy(a:lines), 'strdisplaywidth(v:val)'))
    let l:height = min([len(a:lines), 10])
    call nvim_win_set_width(w:preview['id'], l:width)
    call nvim_win_set_height(w:preview['id'], l:height)

    call nvim_buf_set_lines(w:preview['buffer'], 0, -1, v:false, a:lines)
    call nvim_buf_set_option(w:preview['buffer'], 'modified', v:false)
    call nvim_buf_set_option(w:preview['buffer'], 'modifiable', v:false)
endfunction

function! s:Create(options) abort
    let l:buffer = nvim_create_buf(v:false, v:false)
    let l:winid = nvim_open_win(l:buffer, v:false, {
    \ 'relative': 'cursor',
    \ 'row': 1,
    \ 'col': 0,
    \ 'width': 42,
    \ 'height': 4,
    \ 'style': 'minimal'
    \ })
    call nvim_buf_set_option(l:buffer, 'buftype', 'acwrite')
    call nvim_buf_set_option(l:buffer, 'bufhidden', 'delete')
    call nvim_buf_set_option(l:buffer, 'swapfile', v:false)
    call nvim_buf_set_option(l:buffer, 'filetype', get(a:options, 'filetype', 'ale-preview'))

    let w:preview = {'id': l:winid, 'buffer': l:buffer}
endfunction

function! s:Close() abort
    if !exists('w:preview')
        return
    endif

    call setbufvar(w:preview['buffer'], '&modified', 0)

    if win_id2win(w:preview['id']) > 0
        execute win_id2win(w:preview['id']).'wincmd c'
    endif

    unlet w:preview
endfunction

