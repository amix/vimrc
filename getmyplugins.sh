#!/bin/bash
vimdir=$(pwd)
pushd $vimdir/my_plugins > /dev/null
while IFS='' read -r line || [[ -n "$line"  ]]; do
    echo "Cloning $line"
    git clone $line
done < "$vimdir/mypluginList"
popd > /dev/null

if [ -d $vimdir/my_plugins/YouCompleteMe ]; then
    pushd $vimdir/my_plugins/YouCompleteMe > /dev/null
    git submodule update --init --recursive
    python install.py
    popd > /dev/null
fi

