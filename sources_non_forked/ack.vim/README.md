# ack.vim

Run your favorite search tool from Vim, with an enhanced results list.

This plugin was designed as a Vim frontend for the Perl module [App::Ack]. Ack
can be used as a replacement for 99% of the uses of _grep_. The plugin allows
you to run ack from vim, and shows the results in a split window.

But here's a little secret for the Vim-seasoned: it's just a light wrapper for
Vim's [grepprg] and the [quickfix] window for match results. This makes it easy
to integrate with your own Vim configuration and use existing knowledge of core
features. It also means the plugin is flexible to use with other search tools.

[App::Ack]: http://search.cpan.org/~petdance/ack/ack
[grepprg]: http://vimdoc.sourceforge.net/htmldoc/options.html#'grepprg'
[quickfix]: http://vimdoc.sourceforge.net/htmldoc/quickfix.html#quickfix

## Installation

### Ack

You will need ack (>= 2.0), of course. To install it follow the
[manual](http://beyondgrep.com/install/).

### The Plugin

It is recommended to use one of the popular plugin managers for Vim. There are
many and you probably already have a preferred one, but a few examples for your
copy-and-paste convenience:

#### Vundle

    Plugin 'mileszs/ack.vim'

#### NeoBundle

    NeoBundle 'mileszs/ack.vim'

#### Manual (not recommended)

[Download][releases] the plugin and extract it in `~/.vim/` (or
`%PROGRAMFILES%/Vim/vimfiles` on Windows).

[zipball]: https://github.com/mileszs/ack.vim/archive/master.zip

## Usage

    :Ack [options] {pattern} [{directories}]

Search recursively in `{directories}` (which defaults to the current directory)
for the `{pattern}`.

Files containing the search term will be listed in the quickfix window, along
with the line number of the occurrence, once for each occurrence. `<Enter>` on
a line in this window will open the file, and place the cursor on the matching
line.

Just like where you use `:grep`, `:grepadd`, `:lgrep`, and :`lgrepadd`, you can
use `:Ack`, `:AckAdd`, `:LAck`, and `:LAckAdd` respectively. (See `:help Ack`
after installing, or [`doc/ack.txt`][doc] in the repo, for more information.)

For more ack help see [ack documentation](http://beyondgrep.com/documentation/).

[doc]: https://github.com/mileszs/ack.vim/blob/master/doc/ack.txt

### Keyboard Shortcuts

The quickfix results window is augmented with these convenience mappings:

    ?    a quick summary of these keys, repeat to close
    o    to open (same as Enter)
    O    to open and close the quickfix window
    go   to preview file, open but maintain focus on ack.vim results
    t    to open in new tab
    T    to open in new tab without moving to it
    h    to open in horizontal split
    H    to open in horizontal split, keeping focus on the results
    v    to open in vertical split
    gv   to open in vertical split, keeping focus on the results
    q    to close the quickfix window

### Gotchas

Some characters have special meaning, and need to be escaped in your search
pattern. For instance, `#`. You need to escape it with `:Ack '\\\#define
foo'` to search for '#define foo'. See [issue #5].

[issue #5]: https://github.com/mileszs/ack.vim/issues/5

## Possibly FAQ

#### Can I use `ag` ([The Silver Searcher]) with this?

Absolutely, and probably other tools if their output is similar or you can
write a pattern match for it--just set `g:ackprg`. If you like, you can fall
back to Ack in case you use your vimrc on a system without Ag available:

```vim
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif
```

Since Ack is quite portable you might check a copy of it into your dotfiles
repository in `~/bin` so you'll nearly always have it available.

#### What's the difference from ag.vim?

Well... not a lot really.

Present maintainer, yours truly, [kind of wishes they never forked][sadface],
contributes to both, and wouldn't mind seeing them merged again. ag.vim got a
nice code clean-up (which ack.vim is now hopefully getting), and ack.vim picked
up a few features that haven't made their way to ag.vim, like `:AckWindow`,
optional background search execution with [vim-dispatch], and auto-previewing.

[The Silver Searcher]: https://github.com/ggreer/the_silver_searcher
[sadface]: https://github.com/mileszs/ack.vim/commit/d97090fb502d40229e6976dfec0e06636ba227d5#commitcomment-5771145

## Changelog

Please see [the Github releases page][releases].

### 1.0.9 (unreleased)

* Fix location list and layout of quickfix when using Dispatch (#154)
* Fix the quick help overlay clobbering the list mappings
* Fix `:AckFile` when using Dispatch
* Restore original `'makeprg'` and `'errorformat'` when using Dispatch
* Internal refactoring and clean-up

## Credits

This plugin is derived from Antoine Imbert's blog post [Ack and Vim
Integration][] (in particular, the function in the update to the post). [Miles
Sterrett][mileszs] packaged it up as a plugin and documented it in Vim's help
format, and since then [many contributors][contributors] have submitted
enhancements and fixes.

And of course, where would we be without [Ack]. And, you know, Vim.

[Ack and Vim Integration]: http://blog.ant0ine.com/typepad/2007/03/ack-and-vim-integration.html
[mileszs]: https://github.com/mileszs
[contributors]: https://github.com/mileszs/ack.vim/graphs/contributors
[Ack]: http://beyondgrep.com/

[vim-dispatch]: https://github.com/tpope/vim-dispatch
[releases]: https://github.com/mileszs/ack.vim/releases
