" =============================================================================
" Filename: autoload/lightline/colorscheme/PaperColor.vim
" Author: TKNGUE
" License: MIT License
" Last Change: 2015/07/28 07:35:00.
" =============================================================================

if &background ==# 'light'
  let g:lightline#colorscheme#PaperColor#palette = g:lightline#colorscheme#PaperColor_light#palette
else
  let g:lightline#colorscheme#PaperColor#palette = g:lightline#colorscheme#PaperColor_dark#palette
endif
