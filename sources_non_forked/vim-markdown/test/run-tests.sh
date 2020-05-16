#!/usr/bin/env bash

# Exit on error.
set -e

cd "$( dirname "${BASH_SOURCE[0]}" )"

for dep in ../build/tabular ../build/vim-toml ../build/vim-json ../build/vader.vim; do
  if [[ ! -d $dep ]]; then
    echo "Missing dependency: $dep"
    echo "You may just want to use 'make test'."
    exit 1
  fi
done

vim -Nu vimrc -c 'Vader! *' > /dev/null
