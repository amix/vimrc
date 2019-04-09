colorscheme snazzy
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|tmp\|*.log'
let g:lightline = {
\ 'colorscheme': 'snazzy',
\ 'active': {
\   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'absolutepath', 'modified' ] ],
\  }
\ }
let g:ctrlp_show_hidden = 0
setlocal foldmethod=manual
set number
set foldlevel=99999
set shiftwidth=2
set tabstop=2
set nofoldenable
map <C-W>` gg=G<C-o><C-o>
