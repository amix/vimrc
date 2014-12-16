" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !exists('*bufferline#get_status_string')
  finish
endif

let s:overwrite = get(g:, 'airline#extensions#bufferline#overwrite_variables', 1)

function! airline#extensions#bufferline#init(ext)
  if s:overwrite
    highlight bufferline_selected gui=bold cterm=bold term=bold
    highlight link bufferline_selected_inactive airline_c_inactive
    let g:bufferline_inactive_highlight = 'airline_c'
    let g:bufferline_active_highlight = 'bufferline_selected'
    let g:bufferline_active_buffer_left = ''
    let g:bufferline_active_buffer_right = ''
    let g:bufferline_separator = g:airline_symbols.space
  endif

  call airline#parts#define_raw('file', '%{bufferline#refresh_status()}'.bufferline#get_status_string())
endfunction

