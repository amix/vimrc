" Close the ALEPreviewWindow window with the q key.
noremap <buffer> q :q!<CR>
" Disable some keybinds for the selection window.
noremap <buffer> v <NOP>
noremap <buffer> i <NOP>
noremap <buffer> I <NOP>
noremap <buffer> <C-q> <NOP>
noremap <buffer> <C-v> <NOP>
noremap <buffer> <S-v> <NOP>
noremap <buffer> a <NOP>
noremap <buffer> A <NOP>
noremap <buffer> o <NOP>
noremap <buffer> O <NOP>
" Keybinds for opening selection items.
noremap <buffer> <CR> :call ale#preview#OpenSelectionInBuffer()<CR>
noremap <buffer> t :call ale#preview#OpenSelectionInTab()<CR>
