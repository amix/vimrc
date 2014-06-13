# vim-javascript v0.9.0

JavaScript bundle for vim, this bundle provides syntax and indent plugins.

## A Quick Note on Regexes

Vim 7.4 was released recently, and unfortunately broke how this plugin
handles regexes. There was no real easy way for us to fix this unless we
completely rewrote how regexes work.

Good News: There was a recent update to Vim 7.4 that fixes this issue.

Make sure you are at least using Vim 7.4, with patches 1-7.

If you are stuck on an older version of Vim 7.4 with no way to update,
then simply perform the following commands to fix your current buffer:

```
:set regexpengine=1
:syntax enable
```

## Installation

### Install with [Vundle](https://github.com/gmarik/vundle)

Add to vimrc:

    Bundle "pangloss/vim-javascript"

And install it:

    :so ~/.vimrc
    :BundleInstall

### Install with [pathogen](https://github.com/tpope/vim-pathogen)

      cd ~/.vim/bundle
      git clone https://github.com/pangloss/vim-javascript.git

## Configuration

The following variables control certain syntax highlighting features. You can
add them to your `.vimrc` to enable/disable their features.

#### javascript_enable_domhtmlcss

Enables HTML/CSS syntax highlighting in your JavaScript file.

Default Value: 0

#### b:javascript_fold

Enables JavaScript code folding.

Default Value: 1

#### g:javascript_conceal

Enables concealing characters. For example, `function` is replaced with `ƒ`

Default Value: 0

#### javascript_ignore_javaScriptdoc

Disables JSDoc syntax highlighting

Default Value: 0

## Contributing

This project uses the [git
flow](http://nvie.com/posts/a-successful-git-branching-model/) model for
development. There's [a handy git module for git
flow](//github.com/nvie/gitflow). If you'd like to be added as a contributor,
the price of admission is 1 pull request. Please follow the general code style
guides (read the code) and in your pull request explain the reason for the
proposed change and how it is valuable.

## Bug report

Report a bug on [GitHub Issues](https://github.com/pangloss/vim-javascript/issues).
