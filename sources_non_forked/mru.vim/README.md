![unit-tests](https://github.com/yegappan/mru/workflows/unit-tests/badge.svg?branch=master) ![Coverage Status](https://codecov.io/gh/yegappan/mru/coverage.svg?branch=master)

# Most Recently Used (MRU) Vim plugin

The Most Recently Used (MRU) plugin provides an easy access to a list of 
recently opened/edited files in Vim. This plugin automatically stores the 
file names as you open/edit them in Vim. 

This plugin works with both Vim and Neovim and will work on all the platforms
where Vim/Neovim are supported.  This plugin will work in both console and GUI
Vim. This version of the MRU plugin needs Vim 7.0 and above.

## Installation

You can install this plugin by downloading the .zip or the .tar.gz file for the latest MRU release from the following page:

https://github.com/yegappan/mru/releases/latest

For Vim 8.0 and above, you can expand the .zip file in the following directory (on Unix/Linux/MacOS systems):

    $ mkdir -p $HOME/.vim/pack/downloads/start/mru
    $ cd $HOME/.vim/pack/downloads/start/mru
    $ unzip <downloaded_mru_file.zip>

For Vim 7.4 and before, you can use the following steps (on Unix/Linux/MacOS systems):

    $ mkdir $HOME/.vim
    $ cd $HOME/.vim
    $ unzip <downloaded_mru_file.zip>

You can also install this plugin directly from github using the following steps (for Vim 8.0 and above):

    $ mkdir -p $HOME/.vim/pack/downloads/start/mru
    $ cd $HOME/.vim/pack/downloads/start/mru
    $ git clone https://github.com/yegappan/mru

For NeoVim:

    $ mkdir -p $HOME/.config/nvim/pack/downloads/start/mru
    $ cd $HOME/.config/nvim/pack/downloads/start/mru
    $ git clone https://github.com/yegappan/mru

or you can use any one of the Vim plugin managers ([vim-plug](https://github.com/junegunn/vim-plug), [dein.vim](https://github.com/Shougo/dein.vim), [pathogen](https://github.com/tpope/vim-pathogen), [minpac](https://github.com/k-takata/minpac), [vam](https://github.com/MarcWeber/vim-addon-manager), [volt](https://github.com/vim-volt/volt), [Vundle](https://github.com/VundleVim/Vundle.vim), etc.) to install and manage this plugin.

## Usage
After the plugin is installed, it will automatically start to record the names of all the recently used files in the `$HOME/.vim_mru_files` text file.

To open a file from the recently used file list, enter the following command:

    :MRU

This will open a temporary window with the list of file names in the MRU list where you can press `<Enter>` to open a file.

You can fuzzy search a text in the list of file names, by passing a search text to the `:MRU` command:

    :MRU <search_text>

This will open the MRU window with only the file names fuzzy matching the supplied search string.

The user manual is available at:
https://github.com/yegappan/mru/wiki/User-Manual
