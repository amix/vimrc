# Installation instructions

There are two ways to install the vim-misc plug-in and it's up to you which you prefer, both options are explained below. Please note that below are generic installation instructions while some Vim plug-ins may have external dependencies, please refer to the plug-in's [readme](README.md) for details.

## Installation using a ZIP archive

Unzip the most recent ZIP archive of the [vim-misc](http://peterodding.com/code/vim/downloads/misc.zip) plug-in inside your Vim profile directory (usually this is `~/.vim` on UNIX and `%USERPROFILE%\vimfiles` on Windows), restart Vim and execute the command `:helptags ~/.vim/doc` (use `:helptags ~\vimfiles\doc` instead on Windows).

If you get warnings about overwriting existing files while unpacking the ZIP archive you probably don't need to worry about this because it's most likely caused by files like `README.md`, `INSTALL.md` and `addon-info.json`. If these files bother you then you can remove them after unpacking the ZIP archive, they are not required to use the plug-in.

## Installation using a Vim plug-in manager

If you prefer you can also use [Pathogen](http://www.vim.org/scripts/script.php?script_id=2332), [Vundle](https://github.com/gmarik/vundle) or a similar tool to install and update the [vim-misc](https://github.com/xolox/vim-misc) plug-in using a local clone of the git repository. This takes a bit of work to set up the first time but it makes updating much easier, and it keeps each plug-in in its own directory which helps to keep your Vim profile uncluttered.
