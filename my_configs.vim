""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Taglist 的设置
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let Tlist_Show_Onw_File=1       "只显示当前文件的tags
let Tlist_WinWidth=40           "设置taglist宽度
let Tlist_Exit_OnlyWindow=1     "taglist窗口是最后一个窗口，则退出vim
let Tlist_Use_Right_Window=1     "在Vim窗口右侧显示taglist窗口
nmap tl : Tlist<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" indent-guides 的设置
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:indent_guides_auto_colors=0
let g:indent_guides_guide_size=1
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=red   ctermbg=3
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=green ctermbg=4
hi IndentGuidesOdd guibg=red ctermbg=3
hi IndentGuidesEven guibg=green ctermbg=4

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" cscope 的设置
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>ca :call CscopeFindInteractive(expand('<cword>'))<CR>
nnoremap <leader>l :call ToggleLocationList()<CR>
" s: Find this C symbol
nnoremap  <leader>cs :call CscopeFind('s', expand('<cword>'))<CR>
" g: Find this definition
nnoremap  <leader>cg :call CscopeFind('g', expand('<cword>'))<CR>
" d: Find functions called by this function
nnoremap  <leader>cd :call CscopeFind('d', expand('<cword>'))<CR>
" " c: Find functions calling this function
nnoremap  <leader>cc :call CscopeFind('c', expand('<cword>'))<CR>
" " t: Find this text string
nnoremap  <leader>ct :call CscopeFind('t', expand('<cword>'))<CR>
" " e: Find this egrep pattern
nnoremap  <leader>ce :call CscopeFind('e', expand('<cword>'))<CR>
" " f: Find this file
nnoremap  <leader>cf :call CscopeFind('f', expand('<cword>'))<CR>
" " i: Find files #including this file
nnoremap  <leader>ci :call CscopeFind('i', expand('<cword>'))<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 其他设置
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set number

set cursorcolumn    "高亮光标所在的列
set cursorline      "高两光标所在的行
highlight CursorLine   cterm=NONE ctermbg=black ctermfg=green guibg=NONE guifg=NONE
highlight CursorColumn cterm=NONE ctermbg=black ctermfg=green guibg=NONE guifg=NONE
