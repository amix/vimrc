#!/bin/bash
vimdir=$(pwd)
pushd $vimdir/my_plugins > /dev/null
while IFS='' read -r line || [[ -n "$line"  ]]; do
    echo "Cloning $line"
    git clone $line
done < "$vimdir/mypluginList"
popd > /dev/null
