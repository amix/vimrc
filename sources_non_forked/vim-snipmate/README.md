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

We recommend one of the following methods for installing SnipMate and its
dependencies. SnipMate depends on [vim-addon-mw-utils][mw-utils] and
[tlib][tlib]. Since SnipMate does not ship with any snippets, we suggest
looking at the [vim-snippets][vim-snippets] repository.

* Using [VAM][vam], add `vim-snippets` to the list of packages to be installed.

* Using [Pathogen][pathogen], run the following commands:

        % cd ~/.vim/bundle
        % git clone https://github.com/tomtom/tlib_vim.git
        % git clone https://github.com/MarcWeber/vim-addon-mw-utils.git
        % git clone https://github.com/garbas/vim-snipmate.git

        # Optional:
        % git clone https://github.com/honza/vim-snippets.git

* Using [Vundle][vundle], add the following to your `vimrc` then run
  `:BundleInstall`

        Bundle "MarcWeber/vim-addon-mw-utils"
        Bundle "tomtom/tlib_vim"
        Bundle "garbas/vim-snipmate"

        " Optional:
        Bundle "honza/vim-snippets"

## FAQ ##

> How does SnipMate determine which snippets to load? How can I separate, for
> example, my Rails snippets from my Ruby snippets?

Primarily SnipMate looks at the `'filetype'` and `'syntax'` settings. Taking
"scopes" from these options, it looks in each `snippets/` directory in
`'runtimepath'` for files named `scope.snippets`, `scope/*.snippets`, or
`scope_*.snippets`.

However we understand this may not allow for the flexibility desired by some
languages. For this we provide two options: scope aliases and the
`:SnipMateLoadScope` command. Scope aliases simply say "whenever this scope is
loaded, also load this other scope:

    let g:snipMate = {}
    let g:snipMate.scope_aliases = {}
    let g:snipMate.scope_aliases['ruby'] = 'ruby,rails'

will load the `ruby-rails` scope whenever the `ruby` scope is active. The
`:SnipMateLoadScope foo` command will always load the foo scope in the current
buffer. The [vim-rails](https://github.com/tpope/vim-rails) plugin automatically
does `:SnipMateLoadScope rails` when editing a Rails project for example.

## Release Notes ##

### Master ###

* Implement simple caching
* Remove expansion guards
* Fix bug with mirrors in the first column
* Fix bug with tabs in indents ([#143][143])
* Fix bug with mirrors in placeholders
* Fix reading single snippet files
* Fix the use of the visual map at the end of a line
* Add `:SnipMateLoadScope` command and buffer-local scope aliases
* Load `<scope>_*.snippets` files

### 0.87 - 2014-01-04 ###

* Stop indenting empty lines when expanding snippets
* Support extends keyword in .snippets files
* Fix visual placeholder support
* Add zero tabstop support
* Support negative 'softtabstop'
* Add g:snipMate_no_default_aliases option
* Add <Plug>snipMateTrigger for triggering an expansion inside a snippet
* Add snipMate#CanBeTriggered() function

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

[143]: https://github.com/garbas/vim-snipmate/issues/143
