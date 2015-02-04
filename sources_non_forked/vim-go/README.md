# vim-go

Go (golang) support for Vim. It comes with pre-defined sensible settings (like
auto gofmt on save), has autocomplete, snippet support, improved syntax
highlighting, go toolchain commands, etc...  If needed vim-go installs all
necessary binaries for providing seamless Vim integration with current
commands. It's highly customizable and each individual feature can be
disabled/enabled easily.

![vim-go](https://dl.dropboxusercontent.com/u/174404/vim-go-2.png)

## Features

* Improved Syntax highlighting, such as Functions, Operators, Methods..
* Auto completion support via `gocode`
* Better `gofmt` on save, keeps cursor position and doesn't break your undo
  history
* Go to symbol/declaration with `godef`
* Look up documentation with `godoc` inside Vim or open it in browser.
* Automatically import packages via `goimports`
* Compile and `go build` your package, install it with `go install`
* `go run` quickly your current file/files
* Run `go test` and see any errors in quickfix window
* Create a coverage profile and display annotated source code in browser to see
  which functions are covered.
* Lint your code with `golint`
* Run your code through `go vet` to catch static errors.
* Advanced source analysis tool with `oracle`
* Precise type-safe renaming of identifiers with `gorename`
* List all source files and dependencies
* Checking with `errcheck` for unchecked errors.
* Integrated and improved snippets. Supports `ultisnips` or `neosnippet`
* Share your current code to [play.golang.org](http://play.golang.org)
* On-the-fly type information about the word under the cursor
* Tagbar support to show tags of the source code in a sidebar with `gotags`
* Custom vim text objects, such a `a function` or `inner function`

## Install

First of all, do not use it with other Go plugins. If you use pathogen, just
clone it into your bundle directory:

```bash
$ cd ~/.vim/bundle
$ git clone https://github.com/fatih/vim-go.git
```

For Vundle add this line to your vimrc:

```vimrc
Plugin 'fatih/vim-go'
```
and execute `:PluginInstall` (or `:BundleInstall` for older versions of Vundle)

Please be sure all necessary binaries are installed (such as `gocode`, `godef`,
`goimports`, etc..). You can easily install them with the included
`:GoInstallBinaries` command. Those binaries will be automatically downloaded
and installed to your `$GOBIN` environment (if not set it will use
`$GOPATH/bin`).  It requires `git` and `hg` for fetching the individual Go
packages.

### Optional

* Autocompletion is enabled by default via `<C-x><C-o>`, to get real-time
completion (completion by type) install:
[YCM](https://github.com/Valloric/YouCompleteMe) or
[neocomplete](https://github.com/Shougo/neocomplete.vim).
* To get displayed source code tag informations on a sidebar install
[tagbar](https://github.com/majutsushi/tagbar).
* For snippet feature install:
[ultisnips](https://github.com/SirVer/ultisnips) or
[neosnippet](https://github.com/Shougo/neosnippet.vim).
* Screenshot color scheme is a slightly modified molokai: [fatih/molokai](https://github.com/fatih/molokai).

## Usage

Many of the [features](#features) are enabled by default. There are no
additional settings needed. All usages and commands are listed in
`doc/vim-go.txt`. Just open the help page to see all commands:

    :help vim-go

## Mappings

vim-go has several `<Plug>` mappings which can be used to create custom
mappings. Below are some examples you might find useful:

Show a list of interfaces which is implemented by the type under your cursor
with `<leader>s` 

```vim
au FileType go nmap <Leader>s <Plug>(go-implements)
```

Show type info for the word under your cursor with `<leader>i` (useful if you
have disabled auto showing type info via `g:go_auto_type_info`)

```vim
au FileType go nmap <Leader>i <Plug>(go-info)
```

Open the relevant Godoc for the word under the cursor with `<leader>gd` or open
it vertically with `<leader>gv`

```vim
au FileType go nmap <Leader>gd <Plug>(go-doc)
au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
```

Or open the Godoc in browser

```vim
au FileType go nmap <Leader>gb <Plug>(go-doc-browser)
```

Run commands, such as  `go run` with `<leader>r` for the current file or `go
build` and `go test` for the current package with `<leader>b` and `<leader>t`.
Display a beautiful annotated source code to see which functions are covered
with `<leader>c`.

```vim
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
```

By default the mapping `gd` is enabled which opens the target identifier in
current buffer. You can also open the definition/declaration in a new vertical,
horizontal or tab for the word under your cursor:

```vim
au FileType go nmap <Leader>ds <Plug>(go-def-split)
au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
au FileType go nmap <Leader>dt <Plug>(go-def-tab)
```

Rename the identifier under the cursor to a new name

```vim
au FileType go nmap <Leader>e <Plug>(go-rename)
```

More `<Plug>` mappings can be seen with `:he go-mappings`. Also these are just
recommendations, you are free to create more advanced mappings or functions
based on `:he go-commands`.

## Settings
Below are some settings you might find useful. For the full list see `:he go-settings`.

Disable opening browser after posting to your snippet to `play.golang.org`:

```vim
let g:go_play_open_browser = 0
```

By default vim-go shows errors for the fmt command, to disable it:

```vim
let g:go_fmt_fail_silently = 1
```

Enable goimports to automatically insert import paths instead of gofmt:

```vim
let g:go_fmt_command = "goimports"
```

Disable auto fmt on save:

```vim
let g:go_fmt_autosave = 0
```

By default binaries are installed to `$GOBIN` or `$GOPATH/bin`. To change it:

```vim
let g:go_bin_path = expand("~/.gotools")
let g:go_bin_path = "/home/fatih/.mypath"      "or give absolute path
```

By default syntax-highlighting for Functions, Methods and Structs is disabled.
To change it:
```vim
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
```

## Troubleshooting

### Command not found

If trying to use `:GoDef`,  `:GoInfo` and get a `command not found`, check that you have the binaries installed by using: `:GoInstallBinaries`

Before opening vim, check your current `$PATH`:

	echo $PATH
	
after opening vim, run `:echo $PATH`, the output must be your current `$PATH` + `$PATH/bin` (the location where `:GoInstallBinaries` installed the binaries

If problem persists and you are using maybe 'csh' or other shell, try adding this to your .vimrc:

    set shell=/bin/sh
   


### I'm using Fish shell but have some problems using Vim-go

First environment variables in Fish are applied differently, it should be like:

	set -x GOPATH /your/own/gopath

Second, Vim needs a POSIX compatible shell (more info here:
https://github.com/dag/vim-fish#teach-a-vim-to-fish). If you use Fish to open
vim, it will make certain shell based commands fail (means vim-go will fail
too). To overcome this problem change the default shell by adding the following
into your .vimrc (on the top of the file):

	if $SHELL =~ 'fish'
	  set shell='/bin/sh'
	endif

or

	set shell='/bin/sh'

## Why another plugin?

This plugin/package is born mainly from frustration. I had to re-install my Vim
plugins and especially for Go I had to install a lot of separate different
plugins, setup the necessary binaries to make them work together and hope not
to lose them again. Lots of plugins out there lack proper settings.
This plugin is improved and contains all my fixes/changes that I'm using for
months under heavy go development environment.

Give it a try. I hope you like it. Feel free to contribute to the project.

## Donations

Vim-go is an open source project and I'm working on it on my free times. I'm spending a lot of time and thoughts to make it stable, fixing bugs, adding new features, etc... If you like vim-go and find it helpful, you might give me a gift from some of the books (kindle) I have in my wish list: 

[Amazon.com Fatih's Wish List](http://amzn.com/w/3RUTKZC0U30P6). Thanks!

## Credits

* Go Authors for official vim plugins
* Gocode, Godef, Golint, Oracle, Goimports, Gotags, Errcheck projects and authors of those projects.
* Other vim-plugins, thanks for inspiration (vim-golang, go.vim, vim-gocode, vim-godef)
