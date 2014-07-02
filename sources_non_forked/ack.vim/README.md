# ack.vim

This plugin is a front for the Perl module
[App::Ack](http://search.cpan.org/~petdance/ack/ack).  Ack can be used as a
replacement for 99% of the uses of _grep_.  This plugin will allow you to run
ack from vim, and shows the results in a split window.

## Installation

### Ack

You will need the ack(>= 2.0), of course, to install it follow the
[manual](http://beyondgrep.com/install/)

### The Plugin

To install it is recommended to use one of the popular package managers for Vim,
rather than installing by drag and drop all required files into your `.vim` folder.

#### Manual (not recommended)

Just
[download](https://github.com/mileszs/ack.vim/archive/kb-improve-readme.zip) the
plugin and put it in your `~/.vim/`(or `%PROGRAMFILES%/Vim/vimfiles` on windows)

#### Vundle

    Bundle 'mileszs/ack.vim'

#### NeoBundle

    NeoBundle 'mileszs/ack.vim'

## Usage

    :Ack [options] {pattern} [{directories}]

Search recursively in {directory} (which defaults to the current directory) for
the {pattern}.

Files containing the search term will be listed in the split window, along with
the line number of the occurrence, once for each occurrence.  [Enter] on a line
in this window will open the file, and place the cursor on the matching line.

Just like where you use :grep, :grepadd, :lgrep, and :lgrepadd, you can use
`:Ack`, `:AckAdd`, `:LAck`, and `:LAckAdd` respectively.
(See `doc/ack.txt`, or install and `:h Ack` for more information.)

For more ack options see
[ack documentation](http://beyondgrep.com/documentation/)

### Keyboard Shortcuts

In the quickfix window, you can use:

    o    to open (same as enter)
    O    to open and close quickfix window
    go   to preview file (open but maintain focus on ack.vim results)
    t    to open in new tab
    T    to open in new tab silently
    h    to open in horizontal split
    H    to open in horizontal split silently
    v    to open in vertical split
    gv   to open in vertical split silently
    q    to close the quickfix window

This Vim plugin is derived (and by derived, I mean copied, essentially) from
Antoine Imbert's blog post
[Ack and Vim Integration](http://blog.ant0ine.com/typepad/2007/03/ack-and-vim-integration.html)
(in particular, the function at the bottom of the post).  I added a help file that
provides just enough reference to get you going.  I also highly recommend you
check out the docs for the Perl script 'ack', for obvious reasons:
[ack - grep-like text finder](http://beyondgrep.com/).

### Gotchas

Some characters have special meaning, and need to be escaped your search
pattern. For instance, '#'. You have to escape it like this `:Ack '\\\#define
foo'` to search for '#define foo'. (From blueyed in issue #5.)

## Changelog

### 1.0

* Remove support to ack 1.x
* Start to use a Changelog
* Use `autoload` directory to define functions, instead of `plugin`.
* Add option to auto fold the results(`g:ack_autofold_results`)
* Improve documentation, list all options and shortcuts
* Improve highlight option to work when passes directories or use quotes.
* Add g:ack_mapping
* Add g:ack_default_options
* Add a help toggle `?`(like NERDTree)

### 1.0.1

* Fixes #124. Bug with `g:ack_autofold_results`

### 1.0.2

* Add compatibility with [vim-dispatch](https://github.com/tpope/vim-dispatch)

### 1.0.3

* Fixes #127. Use `&l:hlsearch` instead of `v:hlsearch` to keep compatibility
with versions that does not have this variable.

### 1.0.4

* Fixes #128. Always apply mappings, even when using vim-dispatch.

### 1.0.5

* Fixes #128. Fixes the `errorformat` for ack when using vim-dispatch.
* Do not use vim-dispatch by default. To use vim-dispath must set
`g:ack_use_dispatch`

### 1.0.6

* Fixes highlight function to work when user passes options. Ex.: Ack -i test
  Thank's @mannih. (#131, #134)

### 1.0.7

* Fixes highlight function to work when passes more than one option, or options
with double dashes(--option) Thank's to @MiguelLatorre and @mannih

### 1.0.8

* Fixes (again) highlight, now using negative look behind.
* Change mappings `o` and `O` to behave as documented
