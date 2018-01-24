" editor configurations
set foldlevel=20 " disable folding
set number relativenumber
set cursorline

" solorized color scheme
syntax enable
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


