# vim-go [![Build Status](http://img.shields.io/travis/fatih/vim-go.svg?style=flat-square)](https://travis-ci.org/fatih/vim-go)

<p align="center">
  <img style="float: right;" src="assets/vim-go.png" alt="Vim-go logo"/>
</p>

## Features

This plugin adds Go language support for Vim, with the following main features:

* Build with `:GoBuild`, install with `:GoInstall` or test
  with `:GoTest` (run single tests via `:GoTestFunc`)
* Show test coverage with `:GoCoverage` or in browser with `:GoCoverageBrowser` 
* Goto definition with `:GoDef`
* Quick jump to declarations with `:GoDecls` or `:GoDeclsDir`
* Show documentation with `:GoDoc` inside or in browser with `:GoDocBrowser`
* Quickly execute your current file/files with `:GoRun`
* Advanced source analysis tools utilizing guru, such as `:GoImplements`,
  `:GoCallees`, and `:GoReferrers`
* Change or display `GOPATH` with `:GoPath`
* Multiple 3rd linter support with `:GoMetaLinter`
* Renaming identifiers with `:GoRename`
* Share your code to [play.golang.org](http://play.golang.org) with `:GoPlay`
* Switch between `*.go` and `*_test.go` code with `:GoAlternate`
* Add/Remove tags on struct fields with `:GoAddTags`
* Add import paths via `:GoImport` or remove them with `:GoDrop`
* Custom vim text objects such as `a function (af)` or `inner function (if)`
* ... and many more! Please see [doc/vim-go.txt](doc/vim-go.txt) for more information.


## Install

Master branch is a **development** branch. Please use with caution.
I recommend to use the [**latest stable release**](https://github.com/fatih/vim-go/releases/latest)

Vim-go follows the standard runtime path structure. Below are some helper lines
for popular package managers:

*  [Pathogen](https://github.com/tpope/vim-pathogen)
    * `git clone https://github.com/fatih/vim-go.git ~/.vim/bundle/vim-go`
*  [vim-plug](https://github.com/junegunn/vim-plug)
    * `Plug 'fatih/vim-go'`
*  [Vim packages](http://vimhelp.appspot.com/repeat.txt.html#packages)
    * `git clone https://github.com/fatih/vim-go.git ~/.vim/pack/plugins/start/vim-go`

After installing, please install all necessary binaries. We have a handy
command for it:

```
:GoInstallBinaries
```

for more information please check out the [documentation](doc/vim-go.txt)

## Usage

Official documentation can be found under [doc/vim-go.txt](doc/vim-go.txt). You can display it from within Vim with:

```
:help vim-go
```
We also have an [official vim-go
tutorial](https://github.com/fatih/vim-go-tutorial).

## License

The BSD 3-Clause License - see [`LICENSE`](LICENSE) for more details
