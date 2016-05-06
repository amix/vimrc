" Basic formatting settings
set listchars=tab:»·,trail:·
set list
" Turn on line numbering
set number
syntax on
colorscheme monokai

" Enable folding
set foldmethod=indent
set foldlevel=99

" Adds proper PEP-8 settings for Python files
au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix

au BufNewFile,BufRead *.js, *.html, *.css
    \ set tabstop=2
    \ set softtabstop=2
    \ set shiftwidth=2
