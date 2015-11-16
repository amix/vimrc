" No Uganda message on startup"
set shortmess=I

" Mode shown in the statusline all the time"
set noshowmode

" Block can move outside of bounds"
set virtualedit=block

" Set font according to system
if has("mac") || has("macunix")
    set guifont=Source\ Code\ Pro:h15,Menlo:h15
elseif has("win16") || has("win32")
    set guifont=Source\ Code\ Pro:h12,Bitstream\ Vera\ Sans\ Mono:h11
elseif has("linux")
    set guifont=Source\ Code\ Pro\ h12,Bitstream\ Vera\ Sans\ Mono:h11
elseif has("unix")
    set guifont=Monospace\ 11
endif
" set guifont=Source\ Code\ Pro\ 11

nmap <leader>rb :call DeleteTrailingWS()<cr>

" Filtering through vimgrep results using regular expressions (amix on github)
map <leader>cc :botright cope<cr>
map <leader>co ggVGy:tabnew<cr>:set syntax=qf<cr>pgg
