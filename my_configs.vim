colorscheme snazzy
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|tmp\|*.log'
let g:nord_italic = 1
let g:nord_underline = 1
let g:nord_italic_comments = 1
let g:nord_uniform_status_lines = 1
let g:nord_comment_brightness = 12
let g:nord_cursor_line_number_background = 1
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
