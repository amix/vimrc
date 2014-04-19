set mouse=a
colorscheme molokai
set background=dark
let g:airline_theme = 'molokai'
let g:molokai_original = 1
set smartindent
autocmd BufWritePre * :FixWhitespace
set timeoutlen=2000
set pastetoggle=<F6>
inoremap jk <ESC>
nnoremap ; :
#set colorcolumn=80
" Open Vim, be able to undo
set undodir=$HOME/.vim/undo
set undolevels=1000
set undoreload=10000

" System wide copy paste
set clipboard=unnamedplus

" Make Y behave like other capitals
map Y y$
"
" " Start scrolling 3 lines before the border
set scrolloff=3
"
" " Automatically reread files that have been changed externally
set autoread
"
" " Make ^e and ^y scroll 3 lines instead of 1
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>
"
" " don't move the cursor after pasting
" " (by jumping to back start of previously changed text)
noremap p p`[
noremap P P`[
"
" " Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv

" Turn off the christmas lights
nnoremap <cr> :nohlsearch<cr>
" Allow saving as root by w!!
cmap w!! %!sudo tee > /dev/null %

" Finde merge conflict markers
nmap <silent> <leader>cf <ESC>/\v^[<=>]{7}( .*\|$)<CR>

" Use Marked.app to preview Markdown files...
function! s:setupMarkup()
 nnoremap <leader>p :silent !open -a Marked.app '%:p'<cr>
endfunction

" Navigate splits more easily
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" " These makes j/k move up/down a screen line instead of a physical file line (for wrapped lines)
nmap k gk
nmap j gj

" autocmd BufEnter * if &modifiable | NERDTreeFind | wincmd p | endif

" Easymotion {{{
let g:EasyMotion_do_mapping = 0

nnoremap <silent> <Leader>f      :call EasyMotion#F(0, 0)<CR>
onoremap <silent> <Leader>f      :call EasyMotion#F(0, 0)<CR>
vnoremap <silent> <Leader>f :<C-U>call EasyMotion#F(1, 0)<CR>

nnoremap <silent> <Leader>F      :call EasyMotion#F(0, 1)<CR>
onoremap <silent> <Leader>F      :call EasyMotion#F(0, 1)<CR>
vnoremap <silent> <Leader>F :<C-U>call EasyMotion#F(1, 1)<CR>

onoremap <silent> <Leader>t      :call EasyMotion#T(0, 0)<CR>
onoremap <silent> <Leader>T      :call EasyMotion#T(0, 1)<CR>
" }}}


source ~/.vim_runtime/maximum_awesome_vimrc


set tabstop=2
set shiftwidth=2
set expandtab
set smartindent
set autoindent
