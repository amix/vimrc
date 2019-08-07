# The Ultimate vimrc - Addons Edition

Additional mappings and a few modifications. [EasyMotion](https://github.com/easymotion/vim-easymotion) is included in the plugins.

## How to install the Addons Edition?

### Install for your own user only

    git clone --depth=1 https://github.com/aquaductape/vimrc.git ~/.vim_runtime
    sh ~/.vim_runtime/install_awesome_vimrc.sh

### Install for multiple users

To install for multiple users, the repository needs to be cloned to a location accessible for all the intended users.

    git clone --depth=1 https://github.com/aquaductape/vimrc.git /opt/vim_runtime
    sh /opt/vim_runtime/install_awesome_parameterized.sh /opt/vim_runtime user0 user1 user2
    # to install for all users with home directories
    sh /opt/vim_runtime/install_awesome_parameterized.sh /opt/vim_runtime --all

Naturally, `/opt/vim_runtime` can be any directory, as long as all the users specified have read access.

## What was Added?

Some mappings are commented out

basic.vim

    " Treat long lines as break lines (useful when moving around in them)
    map j gj
    map k gk

    " Remap VIM ESC to normal mode from insert mode using jj
    " only issue when you need to literally type two jj's in insert mode therefore you must type slowly
    imap jj <Esc>

    " Just like traditional Select All, Ctrl+a
    map <C-a> ggVG

    " Remap VIM H to first non-blank character
    map H ^
    " Remap VIM L to last blank character
    map L $

    " Clears highlighting
    map <C-n> :noh<return>

    " Replace word by occurence, press '.' to move to the next occurence which auto replaces with new word. I use it to rename variables. So far I haven't found a mapping that does it by scope reliably like vscode.
    nnoremap gr *``cgn


    " This loop remaps all 'alt key + character' to '\e + character'
    " On my vim(windows, but some other windows users didn't have a problem)
    " it won't recognize the Meta key when Alt is pressed
    " https://vi.stackexchange.com/questions/2350/how-to-map-alt-key/2363

    " for i in range(97,122)
    "     let c = nr2char(i)
    "     execute "set <M-".c.">=\e".c.""
    "     " On the 'j' iteration it would look like this
    "     " --> execute \"set <M-j>=\ej"
    " endfor

    " These two mappings are a quality of life improvement of copy/pasting from the clipboard
    " Effectively this paste map applies the indent within the pasted content from the indent level that you're at when you invoke the pasting
    " http://tilvim.com/2014/03/18/a-better-paste.html
    map <Leader>p :set paste<CR>o<esc>"*]p:set nopaste<cr>
    vmap <Leader>y "+y

plugins_config.vim

    let g:EasyMotion_do_mapping = 0 " Disable default mappings

    " Jump to anywhere you want with minimal keystrokes, with just one key binding.
    " `s{char}{label}`
    nmap s <Plug>(easymotion-overwin-f)
    " or
    " `s{char}{char}{label}`
    " Need one more keystroke, but on average, it may be more comfortable.
    nmap s <Plug>(easymotion-overwin-f2)

    " Turn on case-insensitive feature
    let g:EasyMotion_smartcase = 1

    " JK motions: Line motions
    map <Leader>j <Plug>(easymotion-j)
    map <Leader>k <Plug>(easymotion-k)

    " Open a NERDTree automatically when vim starts up if no files were specified
    autocmd StdinReadPre * let s:std_in=1
    autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

    " Open NERDTree automatically when vim starts up on opening a directory
    autocmd StdinReadPre * let s:std_in=1
    autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif

    " Shortcut to open NERDTree
    map <C-t> :NERDTreeToggle<CR>

    " Close vim if the only window left open is a NERDTree
    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

## What was Changed?

basic.vim

    let mapleader = " "

## Removed

basic.vim

    map <leader>pp :setlocal paste!<cr>

plugins_config.vim

    map <leader>nn :NERDTreeToggle<cr>
    map <leader>nb :NERDTreeFromBookmark<Space>
    map <leader>nf :NERDTreeFind<cr>
