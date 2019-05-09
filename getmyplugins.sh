#!/bin/bash
while IFS='' read -r line || [[ -n "$line"  ]]; do
    echo "Cloning $line"
    git clone $line my_plugins/
done < "mypluginList"
