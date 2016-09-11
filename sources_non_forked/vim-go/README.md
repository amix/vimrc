# vim-go

Go (golang) support for Vim, which comes with pre-defined sensible settings (like
auto gofmt on save), with autocomplete, snippet support, improved syntax
highlighting, go toolchain commands, and more.  If needed vim-go installs all
necessary binaries for providing seamless Vim integration with current
commands. It's highly customizable and each individual feature can be
disabled/enabled easily.

![vim-go](https://dl.dropboxusercontent.com/u/174404/vim-go-2.png)

## Features

* Improved Syntax highlighting with items such as Functions, Operators, Methods.
* Auto completion support via `gocode`
* Better `gofmt` on save, which keeps cursor position and doesn't break your undo
  history
* Go to symbol/declaration with `:GoDef`
* Look up documentation with `:GoDoc` inside Vim or open it in browser
* Automatically import packages via `:GoImport` or plug it into autosave
* Compile your package with `:GoBuild`, install it with `:GoInstall` or test
  them with `:GoTest` (also supports running single tests via `:GoTestFunc`)
* Quickly execute your current file/files with `:GoRun`
* Automatic `GOPATH` detection based on the directory structure (i.e. `gb`
  projects, `godep` vendored projects)
* Change or display `GOPATH` with `:GoPath`
* Create a coverage profile and display annotated source code to see which
  functions are covered with `:GoCoverage`
* Call `gometalinter` with `:GoMetaLinter`, which invokes all possible linters
  (golint, vet, errcheck, deadcode, etc..) and shows the warnings/errors
* Lint your code with `:GoLint`
* Run your code through `:GoVet` to catch static errors
* Advanced source analysis tools utilizing guru, such as `:GoImplements`,
  `:GoCallees`, and `:GoReferrers`
* Precise type-safe renaming of identifiers with `:GoRename`
* List all source files and dependencies
* Unchecked error checking with `:GoErrCheck`
* Integrated and improved snippets, supporting `ultisnips` or `neosnippet`
* Share your current code to [play.golang.org](http://play.golang.org) with `:GoPlay`
* On-the-fly type information about the word under the cursor. Plug it into
  your custom vim function.
* Go asm formatting on save
* Tagbar support to show tags of the source code in a sidebar with `gotags`
* Custom vim text objects such as `a function` or `inner function`
  list.
* Jump to function or type declarations with `:GoDecls` or `:GoDeclsDir`
* A async launcher for the go command is implemented for Neovim, fully async
  building and testing (beta).
* Integrated with the Neovim terminal, launch `:GoRun` and other go commands
  in their own new terminal. (beta)
* Alternate between implementation and test code with `:GoAlternate`

Checkout the official [tutorial](https://github.com/fatih/vim-go-tutorial) 
that goes literally over all features and shows many tips and tricks. It shows 
how to install vim-go and explains many unknown use cases. Recommended for 
beginners as well as advanced users: https://github.com/fatih/vim-go-tutorial

## Install

Master branch is supposed to be a development branch. So stuff here can break
and change.  Please try use always the [latest
release](https://github.com/fatih/vim-go/releases/latest)

Vim-go follows the standard runtime path structure, so I highly recommend to
use a common and well known plugin manager to install vim-go. Do not use vim-go
with other Go oriented vim plugins. For Pathogen just clone the repo. For other
plugin managers add the appropriate lines and execute the plugin's install
command.

*  [Pathogen](https://github.com/tpope/vim-pathogen)
  * `git clone https://github.com/fatih/vim-go.git ~/.vim/bundle/vim-go`
*  [vim-plug](https://github.com/junegunn/vim-plug)
  * `Plug 'fatih/vim-go'`
*  [NeoBundle](https://github.com/Shougo/neobundle.vim)
  * `NeoBundle 'fatih/vim-go'`
*  [Vundle](https://github.com/gmarik/vundle)
  * `Plugin 'fatih/vim-go'`
*  [Vim packages](http://vimhelp.appspot.com/repeat.txt.html#packages) (since Vim 7.4.1528)
  * `git clone https://github.com/fatih/vim-go.git ~/.vim/pack/plugins/start/vim-go`

Please be sure all necessary binaries are installed (such as `gocode`, `godef`,
`goimports`, etc.). You can easily install them with the included
`:GoInstallBinaries` command. If invoked, all necessary binaries will be
automatically downloaded and installed to your `$GOBIN` environment (if not set
it will use `$GOPATH/bin`). Note that this command requires `git` for fetching
the individual Go packages. Additionally, use `:GoUpdateBinaries` to update the
installed binaries.

### Optional

* Autocompletion is enabled by default via `<C-x><C-o>`. To get real-time
completion (completion by type) install:
[neocomplete](https://github.com/Shougo/neocomplete.vim) for Vim or
[deoplete](https://github.com/Shougo/deoplete.nvim) and
[deoplete-go](https://github.com/zchee/deoplete-go) for NeoVim or
[SimpleAutoCmplPop](https://github.com/roxma/SimpleAutoComplPop)
* To display source code tag information on a sidebar install
[tagbar](https://github.com/majutsushi/tagbar).
* For snippet features install:
[neosnippet](https://github.com/Shougo/neosnippet.vim) or
[ultisnips](https://github.com/SirVer/ultisnips).
* Screenshot color scheme is a slightly modified molokai:
  [fatih/molokai](https://github.com/fatih/molokai).
* For a better documentation viewer checkout:
  [go-explorer](https://github.com/garyburd/go-explorer).

## Usage

Many of the plugin's [features](#features) are enabled by default. There are no
additional settings needed. All usages and commands are listed in
`doc/vim-go.txt`. Note that help tags needs to be populated. Check your plugin
manager settings to generate the documentation (some do it automatically).
After that just open the help page to see all commands:

    :help vim-go

## Example Mappings

vim-go has several `<Plug>` mappings which can be used to create custom
mappings. Unless otherwise specified, none of these mappings are enabled
by default. Here some examples you might find useful:

Run commands such as `go run` for the current file with `<leader>r` or `go
build` and `go test` for the current package with `<leader>b` and `<leader>t`
respectively. Display beautifully annotated source code to see which functions
are covered with `<leader>c`.

```vim
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
```

By default the mapping `gd` is enabled, which opens the target identifier in
current buffer. You can also open the definition/declaration, in a new vertical,
horizontal, or tab, for the word under your cursor:

```vim
au FileType go nmap <Leader>ds <Plug>(go-def-split)
au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
au FileType go nmap <Leader>dt <Plug>(go-def-tab)
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

Rename the identifier under the cursor to a new name

```vim
au FileType go nmap <Leader>e <Plug>(go-rename)
```

More `<Plug>` mappings can be seen with `:he go-mappings`. Also these are just
recommendations, you are free to create more advanced mappings or functions
based on `:he go-commands`.

## Settings
Below are some settings you might find useful. For the full list see `:he
go-settings`.

By default syntax-highlighting for Functions, Methods and Structs is disabled.
To change it:
```vim
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
```

Enable goimports to automatically insert import paths instead of gofmt:

```vim
let g:go_fmt_command = "goimports"
```

By default vim-go shows errors for the fmt command, to disable it:

```vim
let g:go_fmt_fail_silently = 1
```

Disable auto fmt on save:

```vim
let g:go_fmt_autosave = 0
```

Disable opening browser after posting your snippet to `play.golang.org`:

```vim
let g:go_play_open_browser = 0
```

By default when `:GoInstallBinaries` is called, the binaries are installed to
`$GOBIN` or `$GOPATH/bin`. To change it:

```vim
let g:go_bin_path = expand("~/.gotools")
let g:go_bin_path = "/home/fatih/.mypath"      "or give absolute path
```

Disable updating dependencies when installing/updating binaries:
```vim
let g:go_get_update = 0
```

### Using with Neovim (beta)

Note: Neovim currently is not a first class citizen for vim-go. You are free
to open bugs but I'm not going to look at them. Even though I'm using Neovim
myself, Neovim itself is still alpha. So vim-go might not work well as good as
in Vim. I'm happy to accept pull requests or very detailed bug reports.


Run `:GoRun` in a new tab, horizontal split or vertical split terminal

```vim
au FileType go nmap <leader>rt <Plug>(go-run-tab)
au FileType go nmap <Leader>rs <Plug>(go-run-split)
au FileType go nmap <Leader>rv <Plug>(go-run-vertical)
```

By default new terminals are opened in a vertical split. To change it

```vim
let g:go_term_mode = "split"
```

By default the testing commands run asynchronously in the background and
display results with `go#jobcontrol#Statusline()`. To make them run in a new
terminal

```vim
let g:go_term_enabled = 1
```

### Using with Syntastic
Sometimes when using both `vim-go` and `syntastic` Vim will start lagging while
saving and opening files. The following fixes this:

```vim
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }
```

Another issue with `vim-go` and `syntastic` is that the location list window
that contains the output of commands such as `:GoBuild` and `:GoTest` might not appear.
To resolve this:

```vim
let g:go_list_type = "quickfix"
```

## More info

Check out the [Wiki](https://github.com/fatih/vim-go/wiki) page for more
information. It includes
[Screencasts](https://github.com/fatih/vim-go/wiki/Screencasts), an [FAQ
section](https://github.com/fatih/vim-go/wiki/FAQ-Troubleshooting), and many
other [various pieces](https://github.com/fatih/vim-go/wiki) of information.

## Donation

People have asked for this for a long time, now you can be a fully supporter by
[being a patron](https://www.patreon.com/fatih)! 

By being a patron, you are enabling vim-go to grow and mature, helping me to
invest in bug fixes, new documentation, and improving both current and future
features. It's completely optional and is just a direct way to support Vim-go's
ongoing development. Thanks!

[https://www.patreon.com/fatih](https://www.patreon.com/fatih)

## Credits

* Go Authors for official vim plugins
* Gocode, Godef, Golint, Guru, Goimports, Gotags, Errcheck projects and
  authors of those projects.
* Other vim-plugins, thanks for inspiration (vim-golang, go.vim, vim-gocode,
  vim-godef)
* [Contributors](https://github.com/fatih/vim-go/graphs/contributors) of vim-go

## License

The BSD 3-Clause License - see `LICENSE` for more details
