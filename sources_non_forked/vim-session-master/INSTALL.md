# Installation instructions

*Please note that the vim-session plug-in requires my vim-misc plug-in which is separately distributed.*

There are two ways to install the vim-session plug-in and it's up to you which you prefer, both options are explained below. Please note that below are generic installation instructions while some Vim plug-ins may have external dependencies, please refer to the plug-in's [readme](README.md) for details.

## Installation using ZIP archives

Unzip the most recent ZIP archives of the [vim-session](http://peterodding.com/code/vim/downloads/session.zip) and [vim-misc](http://peterodding.com/code/vim/downloads/misc.zip) plug-ins inside your Vim profile directory (usually this is `~/.vim` on UNIX and `%USERPROFILE%\vimfiles` on Windows), restart Vim and execute the command `:helptags ~/.vim/doc` (use `:helptags ~\vimfiles\doc` instead on Windows).

If you get warnings about overwriting existing files while unpacking the ZIP archives you probably don't need to worry about this because it's most likely caused by files like `README.md`, `INSTALL.md` and `addon-info.json`. If these files bother you then you can remove them after unpacking the ZIP archives, they are not required to use the plug-in.

## Installation using a Vim plug-in manager

If you prefer you can also use [Pathogen](http://www.vim.org/scripts/script.php?script_id=2332), [Vundle](https://github.com/gmarik/vundle) or a similar tool to install and update the [vim-session](https://github.com/xolox/vim-session) and [vim-misc](https://github.com/xolox/vim-misc) plug-ins using local clones of the git repositories. This takes a bit of work to set up the first time but it makes updating much easier, and it keeps each plug-in in its own directory which helps to keep your Vim profile uncluttered.
