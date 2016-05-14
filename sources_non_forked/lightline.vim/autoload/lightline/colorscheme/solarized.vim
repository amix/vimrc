" =============================================================================
" Filename: autoload/lightline/colorscheme/solarized.vim
" Author: itchyny
" License: MIT License
" Last Change: 2013/08/27 10:24:28.
" =============================================================================
if &background ==# 'light'
  let g:lightline#colorscheme#solarized#palette = g:lightline#colorscheme#solarized_light#palette
else
  let g:lightline#colorscheme#solarized#palette = g:lightline#colorscheme#solarized_dark#palette
endif
