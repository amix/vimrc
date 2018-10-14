#!/bin/bash
set -e

echo 'Installing Awesome Vim from '$1
cd $1

VIMRC="set runtimepath+=$1

source $1/vimrcs/basic.vim
source $1/vimrcs/filetypes.vim
source $1/vimrcs/plugins_config.vim
source $1/vimrcs/extended.vim

try
source $1/my_configs.vim
catch
endtry"

if [ $2 == "--all" ]; then
    USERS=($(ls -l /home | awk '{if(NR>1)print $9}'))
    for user in ${USERS[*]}; do
        homepath=$(eval echo "~$user")
        IFS=''
        echo $VIMRC > ${homepath}/.vimrc
        unset IFS
        echo "Installed the Ultimate Vim configuration for user $user successfully! Enjoy :-)"
    done
    echo "Installed the Ultimate Vim configuration successfully! Enjoy :-)"
    exit 0
else
    SELECTED_USERS=(${@:2})
    echo "Selected users: ${SELECTED_USERS[@]}"
    for user in ${SELECTED_USERS[@]}; do
        homepath=$(eval echo "~$user")
        IFS=''
        echo $VIMRC > ${homepath}/.vimrc
        unset IFS
        echo "Installed the Ultimate Vim configuration for user $user successfully! Enjoy :-)"
    done
    exit 0
fi
