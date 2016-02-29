" add line number
:set number
" :set colorcolumn=80

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Copying/Pasting
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enables copying from VIM to MAC clipboard
"

set clipboard=unnamed
let g:syntastic_javascript_args = "-c ./.eslintrc"


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Window management
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" close and open windows quickly
map <leader>wc <C-W>c
map <leader>wn <C-W>n


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Autotrim trailing spaces on specific files
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType javascript autocmd BufWritePre <buffer> :%s/\s\+$//e

