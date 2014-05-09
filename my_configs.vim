"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => my custom settings 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set number                      " line numbers
set nocompatible                " choose no compatibility with legacy vi
set nowrap                      " don't wrap lines

augroup VimCSS3Syntax
  autocmd!
  autocmd FileType css setlocal iskeyword+=-
augroup END
