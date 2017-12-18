vim-jsx
=======

Syntax highlighting and indenting for JSX.  JSX is a JavaScript syntax
transformer which translates inline XML document fragments into JavaScript
objects.  It was developed by Facebook alongside [React][1].

vim-jsx is _not_ a JavaScript syntax package, so in order to use it, you will
also need to choose a base JS highlighter.  [pangloss/vim-javascript][2] is the
recommended package---it is vim-jsx's "official" dependency, and the only
package against which it is regularly tested.  However, vim-jsx makes a best
effort to support other JavaScript syntax packages, including:
- pangloss/vim-javascript
- jelera/vim-javascript-syntax
- othree/yajs

Notably, the system vim JavaScript syntax is _not_ supported, due to its
over-simplicity.  However, the system XML syntax package is an implicit
dependency.

Vim support for inline XML in JS is remarkably similar to the same for PHP,
which you can find [here][3].

Troubleshooting
---------------

If you're experiencing incorrect highlighting or indenting in your JSX code,
please file a GitHub issue which includes the following:

- A brief affirmation that you've read the README and have installed one of the
  supported dependencies (and the name of the one you're using).

- A minimal ~/.vimrc which repros the issue you're having, as well as both a
  paste and a screenshot of the issue (a paste alone is insufficient, since it
  doesn't illustrate the specific highlighting or indenting problem).  To
  obtain a minimal ~/.vimrc, simply bisect your ~/.vimrc by adding `finish` at
  various points in the file.  (You can likewise bisect your included plugins
  by selectively including only half of them, then a quarter, etc.).

Most of the issues filed result from failures to install vim-javascript or
conflicts with existing JS syntax or indent files---so failing to indicate that
you've ruled those issues out may result in your issue being closed with
minimal comment.

(Please feel free to disregard all this for feature requests.)

Usage
-----

By default, JSX syntax highlighting and indenting will be enabled only for
files with the `.jsx` extension.  If you would like JSX in `.js` files, add

```viml
let g:jsx_ext_required = 0
```

to your .vimrc or somewhere in your include path.  If you wish to restrict JSX
to files with the pre-v0.12 `@jsx React.DOM` pragma, add

```viml
let g:jsx_pragma_required = 1
```

to your .vimrc or somewhere in your include path.

Frequently Asked Questions
--------------------------

- _How come syntax highlighting doesn't work at all?_

This is the only question I'll answer with another question---Which do you
think is more likely: (a) this package fails completely and utterly in serving
its most fundamental purpose, or (b) user error?

- _Why are my end tags colored differently than my start tags?_

vim-jsx is basically the glue that holds JavaScript and XML syntax packages
together in blissful harmony.  This means that any XML syntax defaults carry
over to the XML portions of vim, and it's common for many colorschemes to
highlight start and end tags differently due to the system XML syntax defaults.

- _Syntax highlighting seems to work, but breaks highlighting and indenting
  further down in the file.  What's wrong?_

This often results from trying to enable XML folding in one's `~/.vimrc` (i.e.,
via `let g:xml_syntax_folding = 1`).  vim-jsx does not support syntax folding,
and is not tested with either JavaScript or XML folding enabled.

Installation
------------

### Pathogen

The recommended installation method is via [Pathogen][4].  Then simply execute

    cd ~/.vim/bundle
    git clone https://github.com/mxw/vim-jsx.git

(You can install [vim-javascript][2] in an analogous manner.)

### Vundle

You can also add vim-jsx using [Vundle][5]---just add the following lines to
your `~/.vimrc`:

    Plugin 'pangloss/vim-javascript'
    Plugin 'mxw/vim-jsx'

To install from within vim, use the commands below.

    :so ~/.vimrc
    :PluginInstall

Alternatively, use the command below to install the plugins from the command
line.

    vim +PluginInstall +qall

### Manual Installation

If you have no `~/.vim/after` directory, you can download the tarball or zip
and copy the contents to `~/.vim`.

If you have existing `~/.vim/after` files, copy the syntax and indent files
directly into their respective destinations.  If you have existing after syntax
or indent files for Javascript, you'll probably want to do something like

    mkdir -p ~/.vim/after/syntax/javascript
    cp path/to/vim-jsx/after/syntax/jsx.vim ~/.vim/after/syntax/javascript/jsx.vim
    mkdir -p ~/.vim/after/indent/javascript
    cp path/to/vim-jsx/after/indent/jsx.vim ~/.vim/after/indent/javascript/jsx.vim


[1]: http://facebook.github.io/react/           "React"
[2]: https://github.com/pangloss/vim-javascript "pangloss: vim-javascript"
[3]: https://github.com/mxw/vim-xhp             "mxw: vim-xhp"
[4]: https://github.com/tpope/vim-pathogen      "tpope: vim-pathogen"
[5]: https://github.com/VundleVim/Vundle        "VundleVim: Vundle"
