# SnipMate #

SnipMate aims to provide support for textual snippets, similar to TextMate or
other Vim plugins like [UltiSnips][ultisnips]. For
example, in C, typing `for<tab>` could be expanded to

    for (i = 0; i < count; i++) {
        /* code */
    }

with successive presses of tab jumping around the snippet.

Originally authored by [Michael Sanders][msanders], SnipMate was forked in 2011
after a stagnation in development. This fork is currently maintained by [Rok
Garbas][garbas], [Marc Weber][marcweber], and [Adnan Zafar][ajzafar].


## Installing SnipMate ##

SnipMate depends on [vim-addon-mw-utils][mw-utils] and [tlib][tlib]. We
recommend one of the following ways of installing all three.

* Using [Pathogen][pathogen], run the following commands:

        % cd ~/.vim/bundle
        % git clone https://github.com/tomtom/tlib_vim.git
        % git clone https://github.com/MarcWeber/vim-addon-mw-utils.git
        % git clone https://github.com/garbas/vim-snipmate.git

* Using [VAM][vam], add `vim-snippets` to the list of packages to be installed.

* Using [Vundle][vundle], add the following to your `vimrc` then run
  `:BundleInstall`

        Bundle "MarcWeber/vim-addon-mw-utils"
        Bundle "tomtom/tlib_vim"
        Bundle "garbas/vim-snipmate"

Lastly, since SnipMate does not ship with any snippets, we suggest looking at
the [vim-snippets][vim-snippets] repository.


[ultisnips]: https://github.com/sirver/ultisnips
[msanders]: https://github.com/msanders
[garbas]: https://github.com/garbas
[marcweber]: https://github.com/marcweber
[ajzafar]: https://github.com/ajzafar
[mw-utils]: https://github.com/marcweber/vim-addon-mw-utils
[tlib]: https://github.com/tomtom/tlib_vim
[vim-snippets]: https://github.com/honza/vim-snippets
[vam]: https://github.com/marcweber/vim-addon-manager
[pathogen]: https://github.com/tpope/vim-pathogen
[vundle]: https://github.com/gmarik/vundle
