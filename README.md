# What is this?

This is the vim configurations based on [amix/vimrc](https://github.com/amix/vimrc).

# What have I changed?

* Change syntax checker from [syntastic](https://github.com/vim-syntastic/syntastic) to [ale](https://github.com/w0rp/ale) (which is **async** and needs vim 8).

* Add [indentLine](https://github.com/Yggdroot/indentLine) support. (This feature needs vim 7.3+ with `conceal` compiled)

* Add some javascript related plugins. (mainly for vue development with jsx)

* Add [easymotion](https://github.com/easymotion/vim-easymotion) for (IMHO) better development experience.

* Change [lightline](https://github.com/itchyny/lightline.vim) to [vim-airline](https://github.com/vim-airline/vim-airline).

* Change default plugin manager (or 'runtimepath manager') from [pathogen](https://github.com/tpope/vim-pathogen) to [Vundle](https://github.com/VundleVim/Vundle.vim).

# Something to know about the change of plugin manager

Since I can't figure out how to use local plugin with Vundle properly, I didn't include some plugins used by the original version in the `/sources_forked` directory.

If you come out with how to do this, please let me know and I would be appreciate it.

If you want to use Vundle as your plugin manager with the original configuration, you can check the following 2 files:

1. [./vimrcs/vundle_config.vim](./vimrcs/vundle_config.vim), this file has all the vundle-related setups.

2. [./install_awesome_vimrc.sh](./install_awesome_vimrc.sh), this file add one line to source the `vundle_config.vim` at top of the sources.

# Also

Some personal preferences can be found in [./my_configs.vim](./my_configs.vim).
