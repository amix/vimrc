#!/usr/bin/env bash

set -e

repos=(
  'junegunn/vader.vim'

  # languages
  'cakebaker/scss-syntax.vim'
  'digitaltoad/vim-pug'
  'groenewege/vim-less'
  'kchmck/vim-coffee-script'
  'leafgarland/typescript-vim'
  'slm-lang/vim-slm'
  'wavded/vim-stylus'

  # utility
  'scrooloose/nerdcommenter'
)

cd "$(dirname "$0")/.."
mkdir -p pack/testing/start
cd pack/testing/start

# Add our plugin to the pack.
ln -s ../../.. vim-vue

for repo in ${repos[@]}
do
  git clone https://github.com/$repo.git
done
