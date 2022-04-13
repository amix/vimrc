if [ -f ~/.vim_runtime ]
then
  rm -rf ~/.vim_runtime
  cp -r .vim_runtime ~/
else
  cp -r .vim_runtime ~/
fi

cd ~/.vim_runtime
bash install_awesome_vimrc.sh


