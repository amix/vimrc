source ~/.vim_runtime/plugins_config.vim
let mapleader = ";"

set rnu
highlight CursorLineNr guifg=#050505
set nu
set cin ts=4 sw=4 sts=4 et acd
set fileencodings=utf-8,gb2312,gb18030,gbk,ucs-bom,cp936,latin1
set enc=utf8
set fencs=utf8,gbk,gb2312,gb18030
set cursorline

" for complete
set completeopt=menu,menuone,preview,noselect,noinsert
autocmd vimenter * NERDTree
" autocmd vimenter * call system("touch .tags_root")
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" autocmd TextChangedI * call ale#completion#Queue()
" autocmd TextChangedI * call feedkeys("\<c-x>\<c-o>", 'i')

vnoremap <Leader>y "+y
noremap <Leader>p "+p
" autocmd BufWritePost $MYVIMRC source $MYVIMRC

noremap <Leader>n <c-]>
noremap <Leader>b <c-t>

" Exuberant Ctags
" let g:tagbar_ctags_bin='/usr/bin/ctags'         " Proper Ctags locations
" Universal Ctags
let g:tagbar_ctags_bin='/usr/local/bin/ctags'         " Proper Ctags locations
" let g:tagbar_width=26                           " Default is 40, seems too wide
" noremap <silent> <Leader>y :TagbarToggle      " Display panel with y (or ,y)

" noremap <Up> <Nop>
" noremap <Down> <Nop>
" noremap <Left> <Nop>
" noremap <Right> <Nop>

" nnoremap <f5> :!ctags -R *<CR>
" autocmd BufWritePost * call system("ctags -R *")

set spell
set cinoptions=g0,:0,(0,W4,N-s
" " completeparameter
" inoremap <silent><expr> ( complete_parameter#pre_complete("()")
" smap <c-j> <Plug>(complete_parameter#goto_next_parameter)
" imap <c-j> <Plug>(complete_parameter#goto_next_parameter)
" smap <c-k> <Plug>(complete_parameter#goto_previous_parameter)
" imap <c-k> <Plug>(complete_parameter#goto_previous_parameter)


" Specify a directory for plugins
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim_runtime/my_plugins')

" Make sure you use single quotes

" YouCompleteMe

"let g:ycm_confirm_extra_conf = 0
"let g:ycm_auto_trigger = 0
"" let g:ycm_global_ycm_extra_conf = '~/.vim_runtime/data/.ycm_extra_conf.py'
"let g:ycm_global_ycm_extra_conf = '~/.vim_runtime/my_plugins/YouCompleteMe/.ycm_extra_conf.py'
"" let g:ycm_global_ycm_extra_conf = '~/.vim_runtime/my_plugins/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_confirm_extra_conf/'

"nnoremap <leader>jd :YcmCompleter GoTo<CR>
"nnoremap <leader>y :let g:ycm_auto_trigger=0<CR>                " turn off YCM
"nnoremap <leader>Y :let g:ycm_auto_trigger=1<CR>                "turn on YCM

"" let g:ycm_add_preview_to_completeopt = 0
"" let g:ycm_show_diagnostics_ui = 0
"" let g:ycm_server_log_level = 'info'
"" let g:ycm_min_num_identifier_candidate_chars = 2
"" let g:ycm_collect_identifiers_from_comments_and_strings = 1
"" let g:ycm_complete_in_strings=1
""
"" set completeopt=menu,menuone
"" let g:ycm_key_invoke_completion = '<c-z>'
"" noremap <c-z> <NOP>
"" Apply YCM FixIt
"map <F9> :YcmCompleter FixIt<CR>

"" let g:ycm_semantic_triggers =  {
""             \ 'c,cpp,python,java,go,erlang,perl': ['re!\w{2}'],
""             \ 'cs,lua,javascript': ['re!\w{2}'],
""             \ }

" tenfyzhong/CompleteParameter.vim
"Plug 'tenfyzhong/CompleteParameter.vim'

Plug 'elzr/vim-json'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/vim-easy-align'
Plug 'Chiel92/vim-autoformat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'majutsushi/tagbar'
Plug 'buoto/gotests-vim'
" Plug 'ycm-core/YouCompleteMe' ", { 'do': './install.py --clang-completer' }
" Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' } 

Plug 'universal-ctags/ctags'
Plug 'ludovicchabant/vim-gutentags'

" Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline-themes'
" Plug 'altercation/vim-colors-solarized'
" Plug 'itchyny/lightline.vim'

Plug 'voldikss/vim-translator'
" Initialize plugin system
call plug#end()

" gutentags 搜索工程目录的标志，碰到这些文件/目录名就停止向上一级目录递归
let g:gutentags_project_root = ['.root', '.svn', '.git', '.hg', '.project', '.tags_root']
" 所生成的数据文件的名称
let g:gutentags_ctags_tagfile = '.tags'
" 将自动生成的 tags 文件全部放入 ~/.cache/tags 目录中，避免污染工程目录
let s:vim_tags = expand('~/.cache/tags')
let g:gutentags_cache_dir = s:vim_tags
" 配置 ctags 的参数
let g:gutentags_ctags_extra_args = ['--fields=+niazS', '--extra=+q']
let g:gutentags_ctags_extra_args += ['--c++-kinds=+px']
let g:gutentags_ctags_extra_args += ['--c-kinds=+px']
" 检测 ~/.cache/tags 不存在就新建
if !isdirectory(s:vim_tags)
   silent! call mkdir(s:vim_tags, 'p')
endif

" echodoc 
set noshowmode

" for vim-gitgutter
GitGutterEnable
GitGutterLineHighlightsEnable
GitGutterSignsEnable

" for vim-easy-align
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" for vim-autoformat
noremap <F3> :Autoformat<CR>
