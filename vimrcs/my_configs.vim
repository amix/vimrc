set noshowmode  " mode shown in statusline
set guifont=Source\ Code\ Pro\ 10

nmap <leader>rb :call DeleteTrailingWS()<cr>

" Filtering through vimgrep results using regular expressions (amix on github)
map <leader>cc :botright cope<cr>
map <leader>co ggVGy:tabnew<cr>:set syntax=qf<cr>pgg
