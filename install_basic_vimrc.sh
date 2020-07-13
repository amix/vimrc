#!/bin/sh
set -e

# Save previous vimrc.
if [ -f $HOME/.vimrc ]; then
    mv ~/.vimrc ~/.oldvimrc
    echo "Saved previous vimrc as $HOME/.oldvimrc"
fi
cd ~/.vim_runtime
cat ~/.vim_runtime/vimrcs/basic.vim > ~/.vimrc
echo "Installed the Basic Vim configuration successfully! Enjoy :-)"
