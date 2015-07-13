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
[tlib][tlib].

> **NOTE:** SnipMate does not ship with any snippets out of the box. We suggest
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
  `:PluginInstall`

        Plugin 'MarcWeber/vim-addon-mw-utils'
        Plugin 'tomtom/tlib_vim'
        Plugin 'garbas/vim-snipmate'

        " Optional:
        Plugin 'honza/vim-snippets'

## FAQ ##

> SnipMate doesn't work / My snippets aren't triggering

Try all of the following:

* Check that SnipMate is loaded. This can be done by looking for
  `<Plug>snipMateTrigger` and similar maps in the output of `:imap`.
  Additionally make sure either `<Plug>snipMateTrigger` or
  `<Plug>snipMateNextOrTrigger` is mapped to the key you expect.

* Check that the snippets file you mean to use exists, and that it contains the
  snippet you're trying to expand.

* Check that your snippets file is located inside a `foo/snippets` directory,
  where `foo` is a path listed in your `runtimepath`.

* Check that your snippets file is in scope by either the filetype matching the
  path of the snippet file or the scope explicitly loaded.

* Check if any snippets from your snippets file are available. This can be done
  with the "show available snips" map, by default bound to `<C-R><Tab>` in
  insert mode.

If all of the above check out, please open an issue stating your Vim version,
a sample snippet, and a description of exactly what happens when you try to
trigger a snippet.

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

### 0.88 - 2015-04-04 ###

* Implement simple caching
* Remove expansion guards
* Add `:SnipMateLoadScope` command and buffer-local scope aliases
* Load `<scope>_*.snippets` files
* Use CursorMoved autocmd events entirely

* The nested branch has been merged
    * A new snippet parser has been added. The g:snipmate.version as well as
      version lines in snippet files determines which is used
    * The new parser supports tab stops placed within placeholders,
      substitutions, non-consecutive stop numbers, and fewer ambiguities
    * The stop jumping code has been updated
    * Tests have been added for the jumping code and the new parser

* The override branch has been merged
    * The g:snipMate.override option is added. When enabled, if two snippets
      share the same name, the later-loaded one is kept and the other discarded
    * Override behavior can be enabled on a per-snippet basis with a bang (!) in
      the snippet file
    * Otherwise, SnipMate tries to preserve all snippets loaded

* Fix bug with mirrors in the first column
* Fix bug with tabs in indents ([#143][143])
* Fix bug with mirrors in placeholders
* Fix reading single snippet files
* Fix the use of the visual map at the end of a line
* Fix expansion of stops containing only the zero tab stop
* Remove select mode mappings
* Indent visual placeholder expansions and remove extraneous lines ([#177][177]
  and [#178][178])

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
[177]: https://github.com/garbas/vim-snipmate/issues/177
[178]: https://github.com/garbas/vim-snipmate/issues/178
