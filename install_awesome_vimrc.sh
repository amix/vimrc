cd ~/.vim_runtime

echo 'set rtp+=~/.vim_runtime
call vundle#rc()

source ~/.vim_runtime/basic.vim
source ~/.vim_runtime/filetypes.vim
source ~/.vim_runtime/extended.vim
source ~/.vim_runtime/vundle_plugins.vim
source ~/.vim_runtime/plugins_config.vim

try
source ~/.vim_runtime/my_configs.vim
catch
endtry' > ~/.vimrc

echo "Installed the Ultimate Vim configuration successfully! Enjoy :-)"
