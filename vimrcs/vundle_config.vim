set nocompatible
filetype off
set rtp+=~/.vim_runtime/bundle/Vundle.vim

call vundle#begin()
Plugin 'VundleVim/Vundle.vim' " required for vundle to work

" sources_non_forked
Plugin 'maxbrunsfeld/vim-yankstack'
Plugin 'mileszs/ack.vim'
Plugin 'corntrace/bufexplorer'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'vim-scripts/mayansmoke'
Plugin 'scrooloose/nerdtree'
Plugin 'chr4/nginx.vim'
Plugin 'amix/open_file_under_cursor.vim'
Plugin 'scrooloose/snipmate-snippets'
Plugin 'vim-scripts/tlib'
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'sophacles/vim-bundle-mako'
Plugin 'kchmck/vim-coffee-script'
Plugin 'altercation/vim-colors-solarized'
Plugin 'michaeljsmith/vim-indent-object'
Plugin 'groenewege/vim-less'
Plugin 'tpope/vim-markdown'
Plugin 'therubymug/vim-pyte'
Plugin 'garbas/vim-snipmate'
Plugin 'honza/vim-snippets'
Plugin 'tpope/vim-surround'
Plugin 'terryma/vim-expand-region'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-fugitive'
Plugin 'junegunn/goyo.vim'
Plugin 'amix/vim-zenroom2'
" Plugin 'scrooloose/syntastic' " this don't play will with `w0rp/ale`
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-commentary'
Plugin 'fatih/vim-go'
Plugin 'airblade/vim-gitgutter'
Plugin 'morhetz/gruvbox'
Plugin 'nvie/vim-flake8'
Plugin 'digitaltoad/vim-pug'
Plugin 'itchyny/lightline.vim'
Plugin 'tpope/tpope-vim-abolish'
Plugin 'vim-scripts/mru.vim'

" sources_forked
Plugin 'vim-scripts/peaksea'

" my_plugins
Plugin 'w0rp/ale'
Plugin 'ElmCast/elm-vim'
Plugin 'Yggdroot/indentLine'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'easymotion/vim-easymotion'
Plugin 'pangloss/vim-javascript'
Plugin 'posva/vim-vue'
Plugin 'mxw/vim-jsx'

call vundle#end()

filetype plugin indent on

