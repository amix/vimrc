" Bood vim Config


""""""""""""""""""""""
""""  General  
""""""""""""""""""""""

" Sets how many lines of history VIM has to remember
set history=700

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread

" Uncomment the next line to make Vim more Vi-compatible
" NOTE: debian.vim sets 'nocompatible'.  Setting 'compatible' changes numerous
" options, so any other options should be set AFTER setting 'compatible'.
"set compatible



" With a map leader it's possible to do extra key combinations
" " like <leader>w saves the current file
 let mapleader = ","
 let g:mapleader = ","



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""





" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Turn on the WiLd menu
set wildmenu

" " Ignore compiled files
set wildignore=*.o,*~,*.pyc
"if has("win16") || has("win32")
"    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
"else
"    set wildignore+=.git\*,.hg\*,.svn\*
"endif

"Always show current position
set ruler


" Highlight search results
set hlsearch
"
" " Makes search act like search in modern browsers
set incsearch

"For regular expressions turn magic on
set magic

"Show matching brackets when text indicator is over them
set showmatch 
" How many tenths of a second to blink when matching brackets
set mat=2


" Turn off vim'S horroble REgex
nnoremap / /\v
vnoremap / /\v


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" " Enable syntax highlighting
 syntax enable 
"
try
    colorscheme desert
catch
endtry


set background=dark

"Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" " Use spaces instead of tabs
set expandtab
"
" " Be smart when using tabs ;)
set smarttab
"
" " 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4
"
" " Linebreak on 500 characters
set lbr
set tw=500
"
set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines



"""""""""""""""""""""""""""""
" => Visual mode related
"""""""""""""""""""""""""""""""
" " Visual mode pressing * or # searches for the current selection
" " Super useful! From an idea by Michael Naumann
vnoremap <silent> * :call VisualSelection('f', '')<CR>
vnoremap <silent> # :call VisualSelection('b', '')<CR>



" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif


set viminfo^=%



" Delete trailing white space on save, useful for Python and CoffeeScript ;)

autocmd BufWrite *.py :call DeleteTrailingWS()



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Turn persistent undo on 
" "    means that you can undo even when you close a buffer/VIM
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"try
"   set undodir=~/.vim_runtime/temp_dirs/undodir
"   set undofile
"   catch
"endtry




"  Uncommenting the next line enables syntax highlighting by default.
if has("syntax")
  syntax on
endif


"################
"Python
"################
let python_highlight_all = 1



" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
"if has("autocmd")
"  filetype plugin indent on
"endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
"set showcmd            " Show (partial) command in status line.
"set showmatch          " Show matching brackets.
set ignorecase         " Do case insensitive matching
set smartcase          " Do smart case matching
"set incsearch          " Incremental search
"set autowrite          " Automatically save before commands like :next and :make
"set hidden             " Hide buffers when they are abandoned
"set mouse=a            " Enable mouse usage (all modes)




"Forget to open File with sudo
cmap w!! w !sudo tee % >/dev/null
cmap x!! x !sudo tee % >/dev/null


function! VisualSelection(direction, extra_filter) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.' . a:extra_filter)
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    en
    return ''
endfunction

func! DeleteTrailingWS()
    exe "normal mz"
    %s/\s\+$//ge
    exe "normal `z"
endfunc


