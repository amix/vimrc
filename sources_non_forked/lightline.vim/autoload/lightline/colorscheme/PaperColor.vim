" =============================================================================
" Filename: autoload/lightline/colorscheme/PaperColor.vim
" Author: TKNGUE
" License: MIT License
" Last Change: 2017/11/25 11:13:35.
" =============================================================================

if lightline#colorscheme#background() ==# 'light'
  let g:lightline#colorscheme#PaperColor#palette = g:lightline#colorscheme#PaperColor_light#palette
else
  let g:lightline#colorscheme#PaperColor#palette = g:lightline#colorscheme#PaperColor_dark#palette
endif
