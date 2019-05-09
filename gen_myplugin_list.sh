#!/bin/bash
# File   : getmyconfigs.sh
# License: MIT
# Author : Xinyue Ou <xinyue3ou@gmail.com>
# Date   : 06.01.2019


# if [ -f mypluginList ]; then
#     mv mypluginList mypluginList.bak
# fi

touch mypluginList

output=$(pwd)/mypluginList

for dir in ~/.vim_runtime/my_plugins/*; do
    echo $dir
    if [ -d $dir ]; then
        pushd $dir >> /dev/null
        git config --get remote.origin.url >> $output
        popd >> /dev/null
    fi
done
