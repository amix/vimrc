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
noremap <buffer> <CR> :call ale#preview#OpenSelection()<CR>
noremap <buffer> t :call ale#preview#OpenSelectionInTab()<CR>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'execute')
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> q"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> v"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> i"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> I"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> <C-q>"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> <C-v>"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> <S-v>"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> a"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> A"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> o"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> O"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> <CR>"'
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> t"'
