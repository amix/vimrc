nnoremap <F9> :exe 'NERDTreeToggle'<CR>
"set pastetoggle=<F2>
"for easy copy 
set mouse=

"for outside paste mistake
nnoremap <F2> :set invpaste paste?<CR>
imap <F2> <C-O>:set invpaste paste?<CR>
set pastetoggle=<F2>
"let Tlist_Use_Split_Window = 1
"sudo apt-get install ncurses-term
"export TERM=xterm-256color(.bashrc)
"let g:rehash256 = 1
:set nu
" [set nu!] to close the line number
"colorscheme monokai
colorscheme molokai
let g:molokai_original = 1

"filetype plugin indent on
"autocmd FileType python setlocal et sta sw=2 sts=2
"let NERDTreeWinPos='left'

"vertical indent, use <leader>ig to activatee
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size=1
let g:indent_guides_auto_colors = 0
hi IndentGuidesOdd  guibg=darkgrey   ctermbg=darkgrey
hi IndentGuidesEven guibg=darkgrey ctermbg=darkgrey
inoremap {} {<esc>o}<esc>O
"map <leader>1 :tabnext 1<CR>
"map <leader>2 :tabnext 2<CR>
"map <leader>3 :tabnext 3<CR>
"map <leader>4 :tabnext 4<CR>
"map <leader>5 :tabnext 5<CR>
"map <leader>6 :tabnext 6<CR>
"let g:clang_library_path = '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/'
"highlight OverLength ctermbg=red ctermfg=white guibg=#592929 
"match OverLength /\%81v.\+/
"

" temporary change auto incident in some file which the incidient is not  4
map <leader>2 :set shiftwidth=2 tabstop=2<CR>
map <leader>3 :set shiftwidth=3 tabstop=3<CR>
map <leader>4 :set shiftwidth=4 tabstop=4<CR>
