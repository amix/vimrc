" Close the ALEFixSuggest window with the q key.
noremap <buffer> q :q!<CR>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'execute')
let b:undo_ftplugin .= ' | execute "silent! unmap <buffer> q"'
