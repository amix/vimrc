imap jk <esc>
nmap ; :
nmap <leader>d :e .<cr>

nmap <leader>% :source %<cr>
map <F7> mzgg=G`z
map <leader>h :split
map <leader>v :vsplit

map g<F1> :GoDoc 
map <F2> :GoBuild
map <F3> :GoTest
"set shell=zsh\ -i
map <F4> :shell<CR>
map <F5> :source %<CR>

map <leader>/ :set hlsearch!<CR>

try
    colorscheme delek
endtry

nmap imp iimport<SPACE>"
nmap qqq :quitall<CR>

set complete+=k
set complete+=t

nnoremap <F12>c :exe ':silent !chromium-browser %'<CR>
