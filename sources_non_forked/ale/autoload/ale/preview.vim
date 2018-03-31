" Author: w0rp <devw0rp@gmail.com>
" Description: Preview windows for showing whatever information in.

" Open a preview window and show some lines in it.
function! ale#preview#Show(lines) abort
    silent pedit ALEPreviewWindow
    wincmd P
    setlocal modifiable
    setlocal noreadonly
    setlocal nobuflisted
    setlocal filetype=ale-preview
    setlocal buftype=nofile
    setlocal bufhidden=wipe
    :%d
    call setline(1, a:lines)
    setlocal nomodifiable
    setlocal readonly
endfunction
