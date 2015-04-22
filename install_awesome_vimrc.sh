cd ~/.vim_runtime

echo 'set runtimepath+=~/.vim_runtime

source ~/.vim_runtime/vimrcs/basic.vim
source ~/.vim_runtime/vimrcs/filetypes.vim
source ~/.vim_runtime/vimrcs/plugins_config.vim
source ~/.vim_runtime/vimrcs/extended.vim

try
source ~/.vim_runtime/my_configs.vim
catch
endtry' > ~/.vimrc

#For Ubuntu users
type ctags >/dev/null 2>&1 || { echo `uname -a` | grep [uU]buntu >/dev/null && echo "Install ctags"; sudo apt-get install ctags; }
#For Centos users
type ctags >/dev/null 2>&1 || { echo `uname -a` | grep [Cc]entos >/dev/null && echo "Install ctags"; sudo yum install ctags; }
#Just for fun~
echo -n "Loading..." ;sleep 1;echo -n ".";sleep 1;echo -n ".";sleep 1;echo "."

echo "Installed the Ultimate Vim configuration successfully! Enjoy :-)"
