" No Uganda message on startup"
set shortmess=I

" Mode shown in the statusline all the time"
set noshowmode

" Block can move outside of bounds"
set virtualedit=block

set guifont=Source\ Code\ Pro:h11

nmap <leader>rb :call DeleteTrailingWS()<cr>

" Filtering through vimgrep results using regular expressions (amix on github)
map <leader>cc :botright cope<cr>
map <leader>co ggVGy:tabnew<cr>:set syntax=qf<cr>pgg
