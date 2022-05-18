# vim-elixir

[![Build Status](https://travis-ci.org/elixir-editors/vim-elixir.svg?branch=master)](https://travis-ci.org/elixir-editors/vim-elixir)

[Elixir](http://elixir-lang.org) support for vim

## Description

Features:

* Syntax highlighting for Elixir and EEx files
* Filetype detection for `.ex`, `.exs`, `.eex`, `.heex`, `.leex`, and `.sface` files
* Automatic indentation
* Integration between Ecto projects and [vim-dadbod][] for running SQL queries
  on defined Ecto repositories

## Installation

`vim-elixir` can be installed either with a plugin manager or by directly copying the files into your vim folders (location varies between platforms)

### Plugin Managers

If you are using a plugin manager then add `vim-elixir` the way you would any other plugin:

```bash
# Using vim 8 native package loading
#   http://vimhelp.appspot.com/repeat.txt.html#packages
git clone https://github.com/elixir-editors/vim-elixir.git ~/.vim/pack/my-packages/start/vim-elixir

# Using pathogen
git clone https://github.com/elixir-editors/vim-elixir.git ~/.vim/bundle/vim-elixir
```

```viml
" Using vim-plug
Plug 'elixir-editors/vim-elixir'

" Using Vundle
Plugin 'elixir-editors/vim-elixir'

" Using NeoBundle
NeoBundle 'elixir-editors/vim-elixir'
```

### Manual Installation

If you are not using a package manager then you can use the provided `manual_install.sh` script to copy the files into their respective homes.

Run [./manual_install.sh](manual_install.sh) to copy the contents of each directory in the respective directories inside `~/.vim`.

## Configuration

You must add the following to your `~/.vimrc`:

```
" Enable syntax highlighting
syntax on

" Enables filetype detection, loads ftplugin, and loads indent
" (Not necessary on nvim and may not be necessary on vim 8.2+)
filetype plugin indent on
```

## Notes/Caveats

### `mix format` Integration

We've decided not to include `mix format` integration into `vim-elixir`.
If you'd like to set it up yourself, you have the following options:

* For asynchronous execution of the formatter, have a look at [vim-mix-format](https://github.com/mhinz/vim-mix-format)
* Add it as a `formatprg` (e.g. `setlocal formatprg=mix\ format\ -`)

Why isn't this supported? We've run into two major issues with calling out to `mix format`.
First `mix format` would not work unless your program compiled.
Second `mix format` added an external process dependency to `vim-elixir`.

If someone really wanted to try and add this then we might be able to model it after `vim-go`'s `go fmt` integration
which I think could be acceptable to merge into master.

## Development

### Maintenance Help

`vim-elixir` is looking for new maintainers.
If you get a lot of value from it, know vimscript well, or eager to learn about it then feel free to get in touch with @jbodah (GH issue, elixir-lang Slack)

### Running the Tests

The tests depend on having Ruby installed.
They also depend on a GUI vim (gvim, mvim) with server support.
If you do not have gvim or mvim in your PATH then you can create a `.gvim_path` file in the vim-elixir root directory which specifies the path to the GUI vim executable.

To run the tests: `bundle exec parallel_rspec spec`

### Developing in Docker

You can spawn a container with vim and your development configs using `bin/vim` or `bin/nvim`

### Debugging Indent

```
# Open vim in a container loading this plugin
bin/vim myfile.ex

# Debug statements should be configured to print automatically
# Write/indent some code
:messages

# You should see output like the following:
#   ==> Indenting line 3
#   text = '    _ -> :wowo'
#   testing handler elixir#indent#handle_top_of_file
#   testing handler elixir#indent#handle_starts_with_string_continuation
#   testing handler elixir#indent#handle_following_trailing_binary_operator
#   testing handler elixir#indent#handle_starts_with_pipe
#   testing handler elixir#indent#handle_starts_with_binary_operator
#   testing handler elixir#indent#handle_inside_block
#   pattern matching relative to lnum 2
#   current line contains ->; assuming match definition
#   line 3: elixir#indent#handle_inside_block returned 4
#   1 change; before #1  4 seconds ago
#
# This tells you which line is being inspected as well as which handlers are being run
# and which branches are being exercised by those handlers
```

### Feature Wishlist

Here is a list of features that I think would be great additions to `vim-elixir`:

* Regularly merging `vim-elixir` into `vim` and keeping the sync up-to-date
* Fixing our build so it can run regularly on CI
* Live view support
* Testing .exs files and ensuring feature compatibility between .ex and .exs
* Documentation (e.g. `:h vim-elixir`)
* README docs for various .vimrc options/flags
* Identifying and rewriting tests that conflict with `mix format`
* Fixes for indentation rule edge cases (e.g. `with`, see GH issues for examples)
* Simplifying syntax rules
* Performance optimizations for syntax/indent rules (especially for determining if something is a string)

[vim-dadbod]: https://github.com/tpope/vim-dadbod
