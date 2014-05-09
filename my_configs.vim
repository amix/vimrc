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

" autoreload .vimrc
augroup reload_vimrc " {
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END " }

""""""""""""""""""""""""""""""
" => my custom filetypes
"""""""""""""""""""""""""""""""

" ruby
au FileType ruby,eruby setl ofu=rubycomplete#Complete
au FileType html,xhtml setl ofu=htmlcomplete#CompleteTags
au FileType css setl ofu=csscomplete#CompleteCSS

" markdown
augroup markdown
    au! BufRead,BufNewFile *.mkd   setfiletype mkd
    au! BufRead,BufNewFile *.markdown   setfiletype mkd
augroup END

" Cucumber
autocmd BufNewFile,BufReadPost *.feature,*.story set filetype=cucumber

" haml
autocmd BufNewFile,BufRead *.haml setf haml
autocmd BufNewFile,BufRead *.sass setf sass
autocmd BufNewFile,BufRead *.scss setf scss
