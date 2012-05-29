cd ~/.vim_runtime
git submodule init
git submodule update

echo 'set runtimepath=~/.vim_runtime,~/.vim_runtime/after,\$VIMRUNTIME

source ~/.vim_runtime/vimrcs/basic.vim
source ~/.vim_runtime/vimrcs/filetypes.vim
source ~/.vim_runtime/vimrcs/plugins_config.vim
source ~/.vim_runtime/vimrcs/extended.vim

if filereadable("~/.vim_runtime/vimrcs/my_configs.vim")
source ~/.vim_runtime/vimrcs/my_configs.vim
endif' > ~/.vimrc

echo "Installed the Ultimate Vim configuration successfully! Enjoy :-)"
