if has("mac") || has("macunix")
    set gfn=Hack:h10,Source\ Code\ Pro:h10,Menlo:h10
elseif has("win16") || has("win32")
    set gfn=Hack:h10,Source\ Code\ Pro:h10,Bitstream\ Vera\ Sans\ Mono:h10
elseif has("gui_gtk2")
    set gfn=Hack\ 10,Source\ Code\ Pro\ 10,Bitstream\ Vera\ Sans\ Mono\ 10
elseif has("linux")
    set gfn=Hack\ 10,Source\ Code\ Pro\ 10,Bitstream\ Vera\ Sans\ Mono\ 10
elseif has("unix")
    set gfn=Monospace\ 10
endif

" move around tabs. conflict with the original screen top/bottom
" comment them out if you want the original H/L
" go to prev tab 
map <S-H> gT
" go to next tab
map <S-L> gt
" mv current tab left
map <C-H> :execute 'tabmove ' . (tabpagenr()-2)<CR>
" mv current tab right
map <C-L> :execute 'tabmove ' . (tabpagenr()+1)<CR>

" new tab
map <C-t><C-t> :tabnew<CR>
" close tab
map <C-t><C-w> :tabclose<CR> 
" show all open tabs
map <C-t><C-a> :tabs<CR> 

" toggle NerdTree
map <F7> :NERDTreeToggle<CR>
" toggle TagBar
nmap <F8> :TagbarToggle<CR>

set expandtab
set shiftwidth=2
set tabstop=2
set softtabstop=2

set cmdheight=1
set number

" yank to the system register (*) by default 
set clipboard=unnamed

" Mark, highlight multiple words
source ~/.vim_runtime/sources_non_forked/Mark/plugin/mark.vim
let g:mwDefaultHighlightingPalette = 'maximum' 
let g:mwDefaultHighlightingNum = 10

" Vim-jsx
let g:jsx_ext_required = 0

" Eslint
" let g:syntastic_javascript_checkers = ['eslint']

" disable Syntastic by default
let g:syntastic_mode_map = { 'mode': 'passive' }

" setl foldmethod=manual

