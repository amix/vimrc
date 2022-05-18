set runtimepath+=/root/vim-elixir

runtime ftdetect/elixir.vim

filetype plugin indent on

set ruler
set hidden

let g:elixir_indent_debug=1

let mapleader=","

map <leader>syn :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
      \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
      \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
