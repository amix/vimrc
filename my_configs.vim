" How can I open a NERDTree automatically when vim starts up if no files were   specified?
 
 autocmd StdinReadPre * let s:std_in=1
 autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
 
 " How can I map a specific key or shortcut to open NERDTree?
 map <C-n> :NERDTreeToggle<CR>
 
 " How can I close vim if the only window left open is a NERDTree?
 autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.   isTabTree()) | q | endif


"  Easier split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
