This is a Vim extension that emulates iA Writer environment when editing Markdown, reStructuredText or text files.

It requires [goyo.vim](https://github.com/junegunn/goyo.vim) and it works by setting global Goyo callbacks that triggers special setup for Markdown, reStructuredText and text files.

Please note that this might not work perfectly with your colorscheme. Patches are welcome to fix this :-)

## Installation and usage

* Install [goyo.vim](https://github.com/junegunn/goyo.vim)
* In command mode type :Goyo

Additionally you may want to have a shortcut. Add this to your vimrc:

    nnoremap <silent> <leader>z :Goyo<cr>

## Inspirations/Similar

* [Writing Markdown With Style in Vim](http://astrails.com/blog/2013/8/12/writing-markdown-with-style-in-vim)
* [lite-dfm](https://github.com/bilalq/lite-dfm)
* [vimroom](https://github.com/mikewest/vimroom)
* [vim-zenroom](https://github.com/amix/vim-zenroom)
