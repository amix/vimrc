colorscheme dracula 
set relativenumber
autocmd BufNewFile *.cpp 0r ~/.vim/templates/skeleton.cpp
autocmd bufnewfile *.cpp exe "1," . 10 . "g/Date  :-.*/s//Date  :- " .strftime("%d-%m-%Y")

