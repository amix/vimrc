# The Ultimate vimrc - My-Addons Edition

Additional mappings and a few modifications. [EasyMotion](https://github.com/easymotion/vim-easymotion) is included in the plugins.

## How to install the My-Addons branch?

### Install for your own user only

    git clone -b my-addons --single-branch --depth=1 https://github.com/aquaductape/vimrc.git ~/.vim_runtime
    sh ~/.vim_runtime/install_awesome_vimrc.sh

### Install for multiple users

To install for multiple users, the repository needs to be cloned to a location accessible for all the intended users.

    git clone -b my-addons --single-branch --depth=1 https://github.com/aquaductape/vimrc.git /opt/vim_runtime
    sh ~/.vim_runtime/install_awesome_parameterized.sh /opt/vim_runtime user0 user1 user2
    # to install for all users with home directories
    sh ~/.vim_runtime/install_awesome_parameterized.sh /opt/vim_runtime --all
