# vim-go [![GitHub Actions Status](https://github.com/fatih/vim-go/workflows/test/badge.svg)](https://github.com/fatih/vim-go/actions)



<p align="center">
  <img style="float: right;" src="assets/vim-go.png" alt="Vim-go logo"/>
</p>

## Features

This plugin adds Go language support for Vim, with the following main features:

* Compile your package with `:GoBuild`, install it with `:GoInstall` or test it
  with `:GoTest`. Run a single test with `:GoTestFunc`).
* Quickly execute your current file(s) with `:GoRun`.
* Improved syntax highlighting and folding.
* Debug programs with integrated [`delve`](https://github.com/go-delve/delve) support with `:GoDebugStart`.
* Completion and many other features support via `gopls`.
* formatting on save keeps the cursor position and undo history.
* Go to symbol/declaration with `:GoDef`.
* Look up documentation with `:GoDoc` or `:GoDocBrowser`.
* Easily import packages via `:GoImport`, remove them via `:GoDrop`.
* Precise type-safe renaming of identifiers with `:GoRename`.
* See which code is covered by tests with `:GoCoverage`.
* Add or remove tags on struct fields with `:GoAddTags` and `:GoRemoveTags`.
* Call [`staticcheck`](https://staticcheck.io/) with `:GoMetaLinter` to invoke all possible linters
  (e.g. `golint`, `vet`, `errcheck`, `deadcode`, etc.) and put the result in the
  quickfix or location list.
* Lint your code with `:GoLint`, run your code through `:GoVet` to catch static
  errors, or make sure errors are checked with `:GoErrCheck`.
* Advanced source analysis tools utilizing `guru`, such as `:GoImplements`,
  `:GoCallees`, and `:GoReferrers`.
* ... and many more! Please see [doc/vim-go.txt](doc/vim-go.txt) for more
  information.
* Integration with [`gopls`](https://github.com/golang/tools/blob/master/gopls/README.md).
* The `gopls` instance can be shared with other Vim plugins.
* Vim-go's use of `gopls` can be disabled and alternative tools can be used when desired.
* Integration with [`Tagbar`](https://github.com/preservim/tagbar) via [`gotags`](https://github.com/jstemmer/gotags).
* Integration with [`Ultisnips`](https://github.com/SirVer/ultisnips) and other snippet engines.

## Install

vim-go requires at least Vim 8.0.1453 or Neovim 0.4.0.

The [**latest stable release**](https://github.com/fatih/vim-go/releases/latest) is the
recommended version to use. If you choose to use the master branch instead,
please do so with caution; it is a _development_ branch.


vim-go follows the standard runtime path structure. Below are some helper lines
for popular package managers:

* [Vim 8 packages](http://vimhelp.appspot.com/repeat.txt.html#packages)
  * `git clone https://github.com/fatih/vim-go.git ~/.vim/pack/plugins/start/vim-go`
* [Neovim packages](https://neovim.io/doc/user/repeat.html#packages)
  * `git clone https://github.com/fatih/vim-go.git ~/.local/share/nvim/site/pack/plugins/start/vim-go`
* [Pathogen](https://github.com/tpope/vim-pathogen)
  * `git clone https://github.com/fatih/vim-go.git ~/.vim/bundle/vim-go`
* [vim-plug](https://github.com/junegunn/vim-plug)
  * `Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }`
* [Vundle](https://github.com/VundleVim/Vundle.vim)
  * `Plugin 'fatih/vim-go'`

You will also need to install all the necessary binaries. vim-go makes it easy
to install all of them by providing a command, `:GoInstallBinaries`, which will
`go install` all the required binaries.

Check out the Install section in [the documentation](doc/vim-go.txt) for more
detailed instructions (`:help go-install`).

## Usage

The full documentation can be found at [doc/vim-go.txt](doc/vim-go.txt). You can
display it from within Vim with `:help vim-go`.

Depending on your installation method, you may have to generate the plugin's
[`help tags`](http://vimhelp.appspot.com/helphelp.txt.html#%3Ahelptags)
manually (e.g. `:helptags ALL`).

We also have a [tutorial](https://github.com/fatih/vim-go/wiki/Tutorial) in the [official vim-go wiki](https://github.com/fatih/vim-go/wiki).

## FAQ and troubleshooting

The FAQ and troubleshooting tips are in the documentation and can be quickly
accessed using `:help go-troubleshooting`. If you believe you've found a bug or
shortcoming in vim-go that is neither addressed by help nor in [existing
issues](https://github.com/fatih/vim-go/issues), please open an issue with
clear reproduction steps. `:GoReportGitHubIssue` can be used pre-populate a lot
of the information needed when creating a new issue.

## Contributing

All PRs are welcome. If you are planning to contribute a large patch or to
integrate a new tool, please create an issue first to get any upfront questions
or design decisions out of the way first.

You can run the tests locally by running `make`. It will lint the VimL for you,
lint the documentation, and run the tests against the minimum required version
of Vim, other versions of Vim that may be critical to support, and Neovim.

## License

The BSD 3-Clause License - see [`LICENSE`](LICENSE) for more details

