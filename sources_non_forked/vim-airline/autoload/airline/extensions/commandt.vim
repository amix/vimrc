" MIT License. Copyright (c) 2013 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !get(g:, 'command_t_loaded', 0)
  finish
endif

function! airline#extensions#commandt#apply(...)
  if bufname('%') ==# 'GoToFile'
    call airline#extensions#apply_left_override('CommandT', '')
  endif
endfunction

function! airline#extensions#commandt#init(ext)
  call a:ext.add_statusline_func('airline#extensions#commandt#apply')
endfunction
