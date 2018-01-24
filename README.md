# What is this?

This is the vim configurations based on [amix/vimrc](https://github.com/amix/vimrc).

# What have I changed?

* Change syntax checker from [syntastic](https://github.com/vim-syntastic/syntastic) to [ale](https://github.com/w0rp/ale) (which is **async** and needs vim 8).

* Add [indentLine](https://github.com/Yggdroot/indentLine) support. (This feature needs vim 7.3+ with `conceal` compiled)

* Add some javascript related plugins. (mainly for vue development with jsx)

* Add [easymotion](https://github.com/easymotion/vim-easymotion) for (IMHO) better development experience.

* Change [lightline](https://github.com/itchyny/lightline.vim) to [vim-airline](https://github.com/vim-airline/vim-airline).

Detail changes can be found in [./my_configs.vim](./my_configs.vim) and [./my_plugins](./my_plugins).
