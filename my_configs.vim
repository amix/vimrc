" General {

  let maplocalleader="\<space>"

  " Fix the color problem in tmux when we set termguicolors
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set background=dark
  set t_Co=256

  set termguicolors " Make terminal color more gorgeous
  set nu            " Enable line number
  set mouse=a       " Enable mouse
  set textwidth=80  " set textwidth

  " Doxygen syntax highlighting for c family
  au BufNewFile,BufReadPost *.c,*.h setlocal syntax=c.doxygen

" }

" Vim UI {"

  colorscheme gruvbox

  set list
  set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespaces
  set colorcolumn=80

" }

" Formatting {

  set nowrap            " Do not wrap long lines
  set autoindent        " Indent at the same level of the previous line
  set nojoinspaces      " Prevents inserting two spaces a join (J)
  set pastetoggle=<F12> " pastetoggle (same indentation on pastes)

  " Affects what happens when you press >>, << or ==. It also affects how
  " automatic indentation works.
  set shiftwidth=2

  " Affects what happens when you press the <TAB> or <BS> keys. Its default
  " value is the same as the value of 'tabstop', but when using indentation
  " without hard tabs or mixed indentation, you want to set it to the same value
  " as 'shiftwidth'. If 'expandtab' is unset, and 'tabstop' is different from
  " 'softtabstop', the <TAB> key will minimize the amount of spaces inserted by
  " using multiples of TAB characters.
  set softtabstop=2

  " 'expandtab' affects what happens when you press the <TAB> key. If
  " 'expandtab' is set, pressing the <TAB> key will always insert 'softtabstop'
  " amount of space characters. Otherwise, the amount of spaces inserted is
  " minimized by using TAB characters.
  set expandtab         " Tabs are spaces, not tabs

  " Specify indentation for each project
  try
  source ~/.vim_runtime/indentation_projects.vim
  catch
  endtry

" }

" Key (re)Mapping {

  " Unmap mappings {

    silent! nunmap <leader>w
    silent! unmap <leader><cr>
    silent! unmap <leader>pp
    silent! unmap <space>
    silent! unmap <C-j>
    silent! unmap <C-k>
    silent! unmap <C-h>
    silent! unmap <C-l>
    silent! unmap <C-n>
    silent! unmap <C-p>
    silent! unmap 0

  " }

  " Mapping "fast" keycodes {
  " See https://vim.fandom.com/wiki/Mapping_fast_keycodes_in_terminal_Vim

    " Set vim keycodes to terminal keycodes
    set <Home>=[1~ <End>=[4~ <BS>=
    set <S-Up>=[1;2A <S-Down>=[1;2B <S-Right>=[1;2C <S-Left>=[1;2D
    set <F13>=[1;6A <F14>=[1;6B <F15>=[1;6C <F16>=[1;6D
    set <S-F3>=[1;2R
    set <S-F6>=[17;2~

    " Mapping "fast" keycodes
    map <F13> <C-S-Up>
    map <F14> <C-S-Down>
    map <F15> <C-S-Right>
    map <F16> <C-S-Left>
    map! <F13> <C-S-Up>
    map! <F14> <C-S-Down>
    map! <F15> <C-S-Right>
    map! <F16> <C-S-Left>

  " }

  " Mapping {

    " Easier turning off highlighting
    nmap <silent> <localleader>; :noh<CR>

    " Change Working Directory to that of the current file
    cmap cwd lcd %:p:h
    cmap cd. lcd %:p:h
    nmap <localleader>cd :cd %:p:h<cr>:pwd<cr>

    " Open scratch
    nmap <localleader>q :e ~/buffer<cr>

    " Spell toggle
    nmap <localleader>ss :setlocal spell!<cr>

    " Easier moving between misspelled words
    nmap <localleader>sn ]s
    nmap <localleader>sp [s

    " Open QuickFix list for the result of Ack
    nmap <localleader>cb :botright cope<cr>

    " Open a new tab and copy the result of Ack to a new buffer
    nmap <localleader>co ggVGy:tabnew<cr>:set syntax=qf<cr>pgg

    " Easier moving between search results
    nmap <F3> :cn<cr>
    nmap <S-F3> :cp<cr>

    " Close QuickFix list
    nmap <localleader>cc :cclose<cr>

    " Easier moving between Ycm Errors and Warnings
    nmap <C-j> :lnext<cr>
    nmap <C-k> :lprevious<cr>

    " Close Location list
    nmap <localleader>lc :lclose<cr>

    " Easier replacing selections
    vnoremap <silent> <localleader>r :call VisualSelection('replace','')<CR>

    " Easier swapping lines
    nnoremap <C-S-Down> :m .+1<CR>==
    nnoremap <C-S-Up> :m .-2<CR>==
    inoremap <C-S-Down> <Esc>:m .+1<CR>==gi
    inoremap <C-S-Up> <Esc>:m .-2<CR>==gi
    vnoremap <C-S-Down> :m '>+1<CR>gv=gv
    vnoremap <C-S-Up> :m '<-2<CR>gv=gv

    " Yank selection to system clipboard
    vmap <localleader>yk :write !clip.exe<CR><CR>

    imap <C-j> <Plug>snipMateTrigger

    " Code folding options
    nmap <localleader>f0 :set foldlevel=0<CR>
    nmap <localleader>f1 :set foldlevel=1<CR>
    nmap <localleader>f2 :set foldlevel=2<CR>
    nmap <localleader>f3 :set foldlevel=3<CR>
    nmap <localleader>f4 :set foldlevel=4<CR>
    nmap <localleader>f5 :set foldlevel=5<CR>
    nmap <localleader>f6 :set foldlevel=6<CR>
    nmap <localleader>f7 :set foldlevel=7<CR>
    nmap <localleader>f8 :set foldlevel=8<CR>
    nmap <localleader>f9 :set foldlevel=9<CR>

    " Map ctrl x ctrl o to ctrl space
    inoremap <C-Space> <C-x><C-o>
    inoremap <C-@> <C-Space>

  " }

" }

" Plugins {

  " Ack {

    " Easier using Ack
    nmap <localleader>a :Ack 

  " }

  " ALE {

    let g:ale_enabled = 0 " Disable ale

  " }

  " BufExplorer {

    " Easier browing buffers
    nmap <C-b> :ToggleBufExplorer<CR>

  "}

  " Clang-format {

    " Set up code style & auto formatting
    let g:clang_format#code_style = "mozilla"
    " let g:clang_format#auto_format = 1

  " }

  " CtrlP {

    " Easier browsing symbols
    nmap <C-p> :CtrlPTag<CR>

  "}

  " DoxygenToolkit {

    let g:DoxygenToolkit_authorName = "Revc Ra"

    " Easier generating documents
    map <localleader>da :DoxAuthor<CR>
    map <localleader>df :Dox<CR>
    " Disble auto-pair
    map <localleader>db :setlocal paste<CR>:DoxBlock<CR><ESC>:setlocal nopaste<CR>a
    map <localleader>dl :DoxLic<CR>

  " }

  " EasyAlign {

    " Define user operator for EasyAlign
    nmap ga <Plug>(EasyAlign)
    xmap ga <Plug>(EasyAlign)

  " }

  " Git-Gutter {

    " If you don't want vim-gitgutter to set up any mappings at all, use this:
    let g:gitgutter_map_keys = 0

    map <localleader>ht :GitGutterToggle<CR>
    map <localleader>hP :GitGutterPreview<CR>
    map <localleader>hn :GitGutterNextHunk<CR>
    map <localleader>hp :GitGutterPrevHunk<CR>
    map <localleader>hs :GitGutterStageHunk<CR>
    map <localleader>hu :GitGutterUndoHunk<CR>

  " }

  " NERDTree {

    " NERT tree show from left
    let g:NERDTreeWinPos = "left"

    nmap <C-e> :NERDTreeToggle<CR>

  " }

  " Rainbow {

     "set to 0 if you want to enable it later via :RainbowToggle
     let g:rainbow_active = 1

  " }

  " Tagbar {

    nmap <localleader>t :TagbarToggle<CR>

  " }

  " Which Key {

    nnoremap <silent> <localleader>      :<c-u>WhichKey '<Space>'<CR>
    nnoremap <silent> <leader> :<c-u>WhichKey  '\'<CR>

  " }

  " Verdin {

    let g:verdin#autocomplete = 0
    let g:verdin#cooperativemode = 1

  " }

  " Yank Stack {

    " Enable cycling through your history of yanks
    nmap <localleader>p <Plug>yankstack_substitute_older_paste
    nmap <localleader>P <Plug>yankstack_substitute_newer_paste

  " }

  " YCM {

    set completeopt-=preview                    " Don't show preview window
    let g:ycm_always_populate_location_list = 1 " Enable using lnext & lprev to navigate errors
    let g:ycm_confirm_extra_conf = 0            " Turn off question

    " IDE functions provided by Ycm
    nmap <S-F6> :YcmCompleter RefactorRename 
    map <localleader>mf :ClangFormat<CR>
    map <localleader>mm :YcmCompleter FixIt<CR>
    map <localleader>mt :YcmCompleter GetType<CR>
    map <localleader>mg :YcmCompleter GoTo<CR>

  " }

" }
