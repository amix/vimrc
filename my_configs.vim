colorscheme snazzy
let g:lightline = {
\ 'colorscheme': 'snazzy',
\ 'active': {
\   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'absolutepath', 'modified' ] ],
\  }
\ }
setlocal foldmethod=manual
set number
set foldlevel=99999
set shiftwidth=2
set tabstop=2
set nofoldenable
set cursorline
map <Leader>d :bufdo bd!<CR>
map <C-W>` gg=G<C-o><C-o>
