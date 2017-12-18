if exists('loaded_minimap')
    finish
endif

let loaded_minimap = 1

command! MinimapToggle call minimap#ToggleMinimap()
command! Minimap call minimap#ShowMinimap()
command! MinimapClose call minimap#CloseMinimap()
command! MinimapUpdate call minimap#UpdateMinimap()

let g:minimap_show =
      \ get( g:, 'minimap_show', '<leader>mm' )
let g:minimap_update =
      \ get( g:, 'minimap_update', '<leader>mu' )
let g:minimap_close =
      \ get( g:, 'minimap_close', '<leader>mc' )
let g:minimap_toggle =
      \ get( g:, 'minimap_toggle', '<leader>mt' )

execute "nnoremap " . " <silent> " .
      \ g:minimap_show . " :Minimap<CR>"
execute "nnoremap " . " <silent> " .
      \ g:minimap_update . " :MinimapUpdate<CR>"
execute "nnoremap " . " <silent> " .
      \ g:minimap_close . " :MinimapClose<CR>"
execute "nnoremap " . " <silent> " .
      \ g:minimap_toggle . " :MinimapToggle<CR>"
