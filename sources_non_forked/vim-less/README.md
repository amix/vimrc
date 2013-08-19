# VIM-LESS

This vim bundle adds syntax highlighting, indenting and autocompletion for the dynamic stylesheet language [LESS](http://lesscss.org).

This bundle is compatible with [vim-css-color](https://github.com/skammer/vim-css-color),
[vim-css3-syntax](https://github.com/hail2u/vim-css3-syntax) and possibly other plugins that place code
in `after/syntax/css.vim` or `after/syntax/css/*.vim`.

![vim-less with vim-css-color and vim-css3-syntax (colorscheme solarized)](https://github.com/lenniboy/vim-less/raw/master/screenshot.png)


## Installing and Using

- Install [pathogen](http://www.vim.org/scripts/script.php?script_id=2332) into `~/.vim/autoload/` and add the
   following line to your `~/.vimrc`:

        call pathogen#infect()

- Make a clone of the `vim-less` repository:

        $ mkdir -p ~/.vim/bundle
        $ cd ~/.vim/bundle
        $ git clone https://github.com/groenewege/vim-less

- OR use [vundle](https://github.com/gmarik/vundle), adding this line to your `~/.vimrc`:

        Bundle 'groenewege/vim-less'

- OR use git submodules:

        $ git submodule add https://github.com/groenewege/vim-less.git bundle/vim-less
        $ git submodule init


### Map
.less to .css , lessc is required.

    nnoremap ,m :w <BAR> !lessc % > %:t:r.css<CR><space>


## Credits

Inspiration from [vim-haml](https://github.com/tpope/vim-haml),
[scss-syntax.vim](https://github.com/cakebaker/scss-syntax.vim) and
[vim-less](https://github.com/lunaru/vim-less)

## License ##

MIT : [groenewege.mit-license.org](http://groenewege.mit-license.org/)
