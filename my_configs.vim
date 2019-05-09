set nu
set nocompatible
let mapleader = "\<Space>"
set cmdheight=1
set foldmethod=indent "set default foldmethod
set cursorline
highlight Cursorline cterm=bold ctermbg=16 
set scrolloff=1
set swapfile
set shortmess=a
" set tabstop=2
" set shiftwidth=2
" set expandtab

" copy and paste
vmap<leader>y mby:silent exec "!rm ~/.vbuf"<cr>:tabnew ~/.vbuf<cr>p:w<cr>:bdelete!<cr>:silent exec "!{ sed -z '$ s@\\n$@@' ~/.vbuf \| xsel; }" <cr>:redraw!<cr>`b
nmap<leader>y mbyiw:silent exec "!rm ~/.vbuf"<cr>:tabnew ~/.vbuf<cr>p:w<cr>:bdelete!<cr>:silent exec "!{ sed -z '$ s@\\n$@@' ~/.vbuf \| xsel; }"<cr>:redraw!<cr>`b
nmap<leader>yy yy:silent exec "!rm ~/.vbuf"<cr>:tabnew ~/.vbuf<cr>pggdd:w<cr>:bdelete!<cr>:silent exec "!{ sed -z '$ s@\\n$@@' ~/.vbuf \| xsel; }"<cr>:redraw!<cr>
nmap<leader>yya mbvG$y:silent exec "!rm ~/.vbuf"<cr>:redraw!<cr>:tabnew ~/.vbuf<cr>pgg:w<cr>:bdelete!<cr>:silent exec "!{ sed -z '$ s@\\n$@@' ~/.vbuf \| xsel; }"<cr>:redraw!<cr>`b
nmap<leader>yyaa mbggvG$y:silent exec "!rm -f ~/.vbuf"<cr>:redraw!<cr>:tabnew ~/.vbuf<cr>pgg:w<cr>:bdelete!<cr>:silent exec "!{ sed -z '$ s@\\n$@@' ~/.vbuf \| xsel; }"<cr>:redraw!<cr>`b
nmap<leader>p :r! sed -z '$ s@\\n$@@' ~/.vbuf<cr>

" auxiliary
nmap<leader>u :diffupdate<cr>
nmap<leader><leader>o :only<cr>
nmap<leader>/ :noh<cr>
nmap<leader>w <C-w>
nmap<leader>q :q<cr>
nmap<leader><leader>w :w<cr>
nmap<leader><leader>wq :wq<cr>
nmap<leader>d diw
nmap<leader><leader>d diwdiwdiwdiw
nmap<C-a> 0
nmap<C-e> $
vmap<C-a> 0
vmap<C-e> $
imap<C-e> <End>
imap<C-a> <Esc>0a<Esc>i
noremap <silent><leader>k <C-i>zz:call HighLightCursor(2)<cr>
noremap <silent><leader>j <C-o>zz:call HighLightCursor(2)<cr>

" if in diff mode, set as diff shortcut, else set as other
if &diff
    map <silent> <leader>1 :diffget 1<CR>:diffupdate<CR>
    map <silent> <leader>2 :diffget 2<CR>:diffupdate<CR>
    map <silent> <leader>3 :diffget 3<CR>:diffupdate<CR>
else
    map <silent><leader>1 1gt
    map <silent><leader>2 2gt
    map <silent><leader>3 3gt
    map <silent><leader>4 4gt
    map <silent><leader>5 5gt
    map <silent><leader>6 6gt
    map <silent><leader>7 7gt
    map <silent><leader>8 8gt
    map <silent><leader>9 9gt
endif

set autochdir
set tags=./tags;,tags;

function! GoToTagWithNewTab()
    :set noignorecase
    let tagWord = expand("<cword>")
    :tabe
    execute "tj ".expand(tagWord)

    let tagFilename = expand('%:t')
    if tagFilename == ''
        :tabclose
        :tabprevious
    else
        :silent! normal jzok
        :call HighLightCursor(2)
    endif
    :set ignorecase
endfunction

function! GoToTagWithNewSplit()
    :set noignorecase
    let tagWord = expand("<cword>")
    :vnew
    execute "tj ".expand(tagWord)

    let tagFilename = expand('%:t')
    if tagFilename == ''
        :q
    else
        :silent! normal jzok
        :call HighLightCursor(2)
    endif
    :set ignorecase
endfunction

function! HighLightCursor(time)
    let s:time_local = a:time
    while 1
        :highlight Cursorline cterm=bold ctermbg=13
        redraw
        sleep 100m
        :highlight Cursorline cterm=bold ctermbg=16
        redraw
        let s:time_local -= 1
        if s:time_local == 0
            break
        endif
        sleep 100m
    endwhile
endfunction

map <silent><Leader>] :call GoToTagWithNewTab()<CR>
map <silent><leader>\ :call GoToTagWithNewSplit()<CR>
map <silent><leader>[ <C-w>}
map <silent><leader>t <C-w>T
map <silent><leader>n :NERDTree<CR>

map <silent><leader>f mtgd
map <silent><leader><leader>f mt<s-#>
map <silent><leader>g :noh<cr>`t :call HighLightCursor(2)<cr>
map <silent><leader>h :call HighLightCursor(2)<cr>

" bookmark
map <silent><leader>a ma
map <silent><leader><leader>a `azz :call HighLightCursor(2)<cr>

" run when start
autocmd VimEnter * call HighLightCursor(2)

" set spell check
map <silent><Leader>c :set spell spelllang=en_us<CR>
autocmd BufRead,BufNewFile *.txt,*.md set spell spelllang=en_us

" set ycm 
" let g:ycm_global_ycm_extra_conf = "/Users/Hans/.vim_runtime/sources_forked/YouCompleteMe/.ycm_extra_conf.py"
" let g:ycm_add_preview_to_completeopt = 0
" let g:ycm_show_diagnostics_ui = 0
" let g:ycm_server_log_level = 'info'
" let g:ycm_min_num_identifier_candidate_chars = 2
" let g:ycm_collect_identifiers_from_comments_and_strings = 1
" let g:ycm_complete_in_strings=1
" let g:ycm_key_invoke_completion = '<c-z>'
" let g:ycm_autoclose_preview_window_after_completion = 0
" let g:ycm_autoclose_preview_window_after_insertion = 1
" set completeopt=menu,menuone,preview
" 
" noremap <c-z> <NOP>
" 
" let g:ycm_semantic_triggers =  {
" 			\ 'c,cpp,cu,python,java,go,erlang,perl,py': ['re!\w{2}'],
" 			\ 'cs,lua,javascript': ['re!\w{2}'],
" 			\ }
" let g:ycm_filetype_blacklist = {
"       \ 'tagbar' : 1,
"       \ 'qf' : 1,
"       \ 'notes' : 1,
"       \ 'unite' : 1,
"       \ 'vimwiki' : 1,
"       \ 'pandoc' : 1,
"       \ 'infolog' : 1,
"       \ 'mail' : 1
"       \}

" automatically paste without format
let &t_SI .= "\<Esc>[?2004h"
let &t_EI .= "\<Esc>[?2004l"

inoremap <special> <expr> <Esc>[200~ XTermPasteBegin()

function! XTermPasteBegin()
    set pastetoggle=<Esc>[201~
    set paste
    return ""
endfunction

" Commenting blocks of code.
autocmd FileType c,cpp,cu,java,scala let b:comment_leader = '// '
autocmd FileType sh,ruby,python   let b:comment_leader = '# '
autocmd FileType conf,fstab       let b:comment_leader = '# '
autocmd FileType tex              let b:comment_leader = '% '
autocmd FileType mail             let b:comment_leader = '> '
autocmd FileType vim              let b:comment_leader = '" '
noremap <silent><leader>. mb:<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>`blll
noremap <silent><leader>, mb:<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>`b

" For smooth motion
if v:version < 705 " Version less than 7.04.15 does not support
    let g:comfortable_motion_no_default_key_mappings = 1
else
    let g:comfortable_motion_scroll_down_key = "j"
    let g:comfortable_motion_scroll_up_key = "k"
    let g:comfortable_motion_no_default_key_mappings = 1
    let g:comfortable_motion_impulse_multiplier = 1  " Feel free to increase/decrease this value.
    nnoremap <silent> <C-d> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * 2)<CR>
    nnoremap <silent> <C-u> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * -2)<CR>
endif

" For move between split window
nnoremap <silent> fh <C-w>h
nnoremap <silent> fj <C-w>j
nnoremap <silent> fk <C-w>k
nnoremap <silent> fl <C-w>l



" Relative line number
" set number relativenumber
" augroup numbertoggle
"   autocmd!
"   autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
"   autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
" augroup END

" Handle url and file
function! HandleURL()
    " substitute \", \', //
    let s:line = substitute(getline("."), "\\\"", " ", "g")
    let s:line = substitute(s:line, "\\\'", " ", "g")
    let s:line = substitute(s:line, "`", " ", "g")
    let s:line = substitute(s:line, "${HOME}", "~", "g")

    let s:uri_http = matchstr(s:line, '[a-z]*:\/\/[^ >,;)]*')
    let s:uri_www = matchstr(s:line, 'www[^ >,;)]*')
    let s:line = substitute(s:line, "//", " ", "")
    let s:uri_path = matchstr(s:line, '\(\./\|\~\|\.\./\|/\)[^ >,;)]*')

    let s:open_list = ["pdf", "jpg", "jpeg", "png", "doc", "docx"]

    if s:uri_http != ""
        silent exec "!open '".s:uri_http."'"
        redraw!
        echo "'".s:uri_http."' opened"
    elseif s:uri_www != ""
        silent exec "!open https://'".s:uri_www."'"
        redraw!
        echo "'https://".s:uri_www."' opened"
    elseif s:uri_path != ""
        let s:uri_path = substitute(s:uri_path, "^\\\~", $HOME, "")
        if isdirectory(s:uri_path)
            silent exec "!open '".s:uri_path."'"
            redraw!
            echo "'".s:uri_path."' opened"
        elseif filereadable(s:uri_path)
            let s:suffix = fnamemodify(s:uri_path, ':e')
            
            if index(s:open_list, s:suffix) == -1
                :IHT
            else
                silent exec "!open '".s:uri_path."'"
            endif
            redraw!
            echo "'".s:uri_path."' opened"
        else
            echo "'".s:uri_path."' does not exists"
        endif
    else
        :IHT
        redraw!
        echo ":IHT"
    endif
endfunction
map <leader>o :call HandleURL()<cr>

function! ToggleErrors()
    if empty(filter(tabpagebuflist(), 'getbufvar(v:val, "&buftype") is# "quickfix"'))
        " No location/quickfix list shown, open syntastic error location panel
        SyntasticCheck
        Errors
        echo "Syntax Check Finished"
    else
        lclose
        SyntasticReset
        echo "Syntax Check Closed"
    endif
endfunction
nnoremap <silent> <C-e> :call ToggleErrors()<CR>
nnoremap <silent> <C-n> :lnext<CR>:call HighLightCursor(1)<cr>
nnoremap <silent> <C-m> :lprevious<CR>:call HighLightCursor(1)<cr>

" vim header
let g:header_field_author = 'Xinyue Ou'
let g:header_field_author_email = 'xinyue3ou@gmail.com'
let g:header_field_modified_by = 0
let g:header_field_modified_timestamp = 0
let g:header_field_license_id = 'MIT'
" No auto header
let g:header_auto_add_header = 0 
map <F4> :AddHeader<CR>

" Nerd Tree
autocmd StdinReadPre * let s:std_in=1
" Automatically toggle when no file specified
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" Automatically toggle when opening up a directory
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif
map <C-n> :NERDTreeToggle<CR>
" Close vim if the only window left open is a NerdTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Color Scheme
set background=dark
colorscheme gruvbox

" NerdCommenter
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

" Set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1

" Add your own custom formats or override the defaults
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }

" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1

" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1

" Enable NERDCommenterToggle to check all selected lines is commented or not 
let g:NERDToggleCheckAllLines = 1
            
" Tab Navigation
nnoremap <silent> th  :tabfirst<CR>
nnoremap <silent> tk  :tabnext<CR>
nnoremap <silent> tj  :tabprev<CR>
nnoremap <silent> tl  :tablast<CR>
nnoremap <silent> tt  :tabedit<Space>
nnoremap <silent> tn  :tabnew<Space>
nnoremap <silent> tm  :tabm<Space>
nnoremap <silent> td  :tabclose<CR>

" Clear highlighting when press esc
nnoremap <esc> :noh<return><esc>
set timeout ttimeout
set timeoutlen=200 ttimeoutlen=50


" Enable gentle auto pair
let g:AutoPairsUseInsertedCount = 1 

" YCM disable preview
set completeopt-=preview

let g:gruvbox_contrast_dark = 'hard'
set background=dark   " Setting light mode

" Terminal mapping
tnoremap <C-h> <C-w>h
tnoremap <C-j> <C-w>j
tnoremap <C-k> <C-w>k
tnoremap <C-l> <C-w>l

nnoremap <silent> ts :terminal<CR>
nnoremap <silent> tv :vert terminal<CR>


" Get rid of the annoying arrow key problems
nnoremap <silent> <ESC>OA <UP>
nnoremap <silent> <ESC>OB <DOWN>
nnoremap <silent> <ESC>OC <RIGHT>
nnoremap <silent> <ESC>OD <LEFT>
inoremap <silent> <ESC>OA <UP>
inoremap <silent> <ESC>OB <DOWN>
inoremap <silent> <ESC>OC <RIGHT>
inoremap <silent> <ESC>OD <LEFT>

" Change the pane size up and down
nnoremap <silent> <C-UP> :resize +1<CR>
nnoremap <silent> <C-DOWN> :resize -1<CR>
nnoremap <silent> <C-LEFT> :vertical resize +1<CR>
nnoremap <silent> <C-RIGHT> :vertical resize -1<CR>
