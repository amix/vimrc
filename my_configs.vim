" editor configurations
set foldlevel=20 " disable folding
set number relativenumber
set cursorline

" solorized color scheme
set background=dark
colorscheme solarized

" set indent preferences based on file type
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2
autocmd Filetype vue setlocal ts=2 sts=2 sw=2
autocmd Filetype vue syntax sync fromstart

" macvim font
if has("gui_macvim")
    set guifont=Operator\ Mono\ Light\ Italic:h18
endif

"
" plugins
"
"
    " nerdtree
    let g:NERDTreeWinPos = "left"

    " ctrlp
    let g:ctrlp_map = '<leader>p'
    let g:ctrlp_max_files = 0
    let g:ctrlp_max_depth = 40
    let g:ctrlp_buffer = '<leader>f'

    " indent line
    let g:indentLine_char = '¦'

    " vim-vue
    let g:vue_disable_pre_processors = 1 " otherwise input will be slow

    " ale
    let g:ale_sign_column_always = 1 " always display that gutter
    let g:airline#extensions#ale#enabled = 1 " display errors or warnings with `vim-airline`

    " airline
    let g:airline_theme = 'solarized'
    let g:airline_solarized_bg = 'dark'
    let g:airline_powerline_fonts = 1
    let g:airline#extensions#tabline#enabled = 1
    let g:airline#extensions#tabline#formatter = 'unique_tail'
