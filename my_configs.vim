" 使用系统剪贴板
set clipboard=unnamedplus
set ts=4
set nu

map q :q<CR>

" map w :w<cr>
" map b <c-v>

map t :tabe<space>

" 使光标位置屏幕中间位置
map j gjzz
map k gkzz
map * *zz
map # #zz
map n nzz
map <S-n> <S-n>zz
map <C-o> <C-o>zz
map <C-i> <C-i>zz

map <C-a> ggVG

" 缩进
map <tab> V>
map <S-tab> V<
map <C-tab> gt

" 切换目录树窗口
map <C-h> :NERDTreeToggle<CR>
" 最近文件
map <C-e> :MRU<CR>


" 切换窗口 
" ^[ = Alt
map w <C-w>w
map h <C-w>h
map j <C-w>j
map k <C-w>k
map l <C-w>l

" 编辑器模式下复制新行
imap <C-d> <Esc>Vypi
