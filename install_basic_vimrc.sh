#!/bin/sh
set -e

cd ~/.vim_runtime
cat ~/.vim_runtime/vimrcs/basic.vim > ~/.vimrc
python update_plugins.py || python3 update_plugins.py
echo "Installed the Basic Vim configuration successfully! Enjoy :-)"
