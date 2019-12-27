colorscheme snazzy
let g:lightline = {
\ 'colorscheme': 'snazzy',
\ 'active': {
\ 'component_function': {
      \   'gitbranch': 'fugitive#head'
      \ },
\   'left': [ [ 'mode', 'paste' ], [ 'gitbranch', 'readonly', 'absolutepath', 'modified' ] ],
\  }
\ }
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }
setlocal foldmethod=manual
set number
set foldlevel=99999
set shiftwidth=2
set tabstop=2
set nofoldenable
set cursorline
set termguicolors
let &t_8f = "\e[38;2;%lu;%lu;%lum"
let &t_8b = "\e[48;2;%lu;%lu;%lum"
map <Leader>d :bufdo bd!<CR>
map <C-W>` gg=G<C-o><C-o>
autocmd FileType vue syntax sync fromstart
