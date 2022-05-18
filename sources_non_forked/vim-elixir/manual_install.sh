#!/usr/bin/env bash

exit_whit_error_message() {
  printf '%s\n' "$1" >&2
  exit 1
}

show_help() {
  echo "The script to install the vim-elixir plugin"
  echo
  echo "Usage: ./manual_install.sh [OPTIONS]"
  echo 
  echo "Options:"
  echo "-o, --output-dir string  The name of the directory where plugin will be installed. By default the output directory name is 'vim-elixir'"
  echo "  Example: ./manual_install.sh -o vim-elixir   # The plugin will be installed in ~/.vim/pack/vim-elixir/start/vim-elixir directory"
  echo "           ./manual_install.sh -o elixir-opts  # The plugin will be installed in ~/.vim/pack/elixir-opts/start/elixir-opts directory"
  echo
}

# Initialize all the option variables.
# This ensures we are not contaminated by variables from the environment.
VIM_PLUGIN_NAME=vim-elixir

while :; do
  case $1 in
    -h|-\?|--help)
      show_help
      exit
      ;;
    -o|--output-dir)
      if [ "$2" ]; then
        VIM_PLUGIN_NAME=$2
        shift
      else
        exit_whit_error_message 'ERROR: "--name" requires a non-empty option argument.'
      fi
      ;;
    --output-dir=?*)
       # Delete everything up to "=" and assign the remainder.
      VIM_PLUGIN_NAME=${1#*=}
      ;;
    --output-dir=) # Handle the case of an empty --name=
      exit_whit_error_message 'ERROR: "--name" requires a non-empty option argument.'
      ;;
    -?*)
      printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2
      ;;
    *)
      # Default case: No more options, so break out of the loop.
      break
  esac

  shift
done

VIM_INSTALL_DIR=~/.vim/pack/$VIM_PLUGIN_NAME/start/$VIM_PLUGIN_NAME

mkdir -p $VIM_INSTALL_DIR

echo "Installing plugin in the ${VIM_INSTALL_DIR} directory"
for DIR in autoload compiler ftdetect ftplugin indent syntax
do
  cp -R $DIR $VIM_INSTALL_DIR
done