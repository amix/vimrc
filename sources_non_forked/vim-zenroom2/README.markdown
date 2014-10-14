This is a Vim extension that emulates iA Writer environment when editing Markdown, reStructuredText or text files.

It requires [goyo.vim](https://github.com/junegunn/goyo.vim) and it works by setting global Goyo callbacks that triggers special setup for Markdown, reStructuredText and text files.

This is an improvement of my initial version called [vim-zenroom](https://github.com/amix/vim-zenroom). Please read more here [
zenroom for Vim: Focusing only on the essential](http://amix.dk/blog/post/19744#zenroom-for-Vim-Focsuing-only-on-the-essential).

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


## How it looks in action

![Screenshot 3](http://amix.dk/uploads/zenroom_documentation.jpg)

![Screenshot 4](http://amix.dk/uploads/zenroom_documentation_1.jpg)
