# rust.vim

## Description

This is a Vim plugin that provides [Rust][r] file detection, syntax highlighting, formatting,
[Syntastic][syn] integration, and more. It requires Vim 8 or higher for full functionality.
Some things may not work on earlier versions. 

## Installation

Use one of the following package managers:

* [Vim8 packages][vim8pack]:
  * `git clone https://github.com/rust-lang/rust.vim ~/.vim/pack/plugins/start/rust.vim`
* [Vundle][v]:
  * Add `Plugin 'rust-lang/rust.vim'` to `~/.vimrc`
  * `:PluginInstall` or `$ vim +PluginInstall +qall`
  * *Note:* Vundle will not automatically detect Rust files properly if `filetype
on` is executed before Vundle. Please check the [quickstart][vqs] for more
details. Errors such as `Not an editor command: RustFmt` may occur if Vundle
is misconfigured with this plugin.
* [Pathogen][p]:
  * `git clone --depth=1 https://github.com/rust-lang/rust.vim.git ~/.vim/bundle/rust.vim`
* [vim-plug][vp]:
  * Add `Plug 'rust-lang/rust.vim'` to `~/.vimrc`
  * `:PlugInstall` or `$ vim +PlugInstall +qall`
* [dein.vim][d]:
  * Add `call dein#add('rust-lang/rust.vim')` to `~/.vimrc`
  * `:call dein#install()`
* [NeoBundle][nb]:
  * Add `NeoBundle 'rust-lang/rust.vim'` to `~/.vimrc`
  * Re-open vim or execute `:source ~/.vimrc`

## Features

### Error checking with [Syntastic][syn]

`rust.vim` automatically registers `cargo` as a syntax checker with
[Syntastic][syn], if nothing else is specified. See `:help rust-syntastic`
for more details.

### Source browsing with [Tagbar][tgbr]

The installation of Tagbar along with [Universal Ctags][uctags] is recommended
for a good Tagbar experience. For other kinds of setups, `rust.vim` tries to
configure Tagbar to some degree.

### Formatting with [rustfmt][rfmt]

The `:RustFmt` command will format your code with
[rustfmt][rfmt] if installed.

Placing `let g:rustfmt_autosave = 1` in your `~/.vimrc` will
enable automatic running of `:RustFmt` when you save a buffer.

Do `:help :RustFmt` for further formatting help and customization
options.

### [Playpen][pp] integration

*Note:* This feature requires [webapi-vim][wav] to be installed.

The `:RustPlay` command will send the current selection, or if
nothing is selected the current buffer, to the [Rust playpen][pp].

If you set g:rust_clip_command RustPlay will copy the url to the clipboard.

- Mac:

        let g:rust_clip_command = 'pbcopy'

- Linux:

        let g:rust_clip_command = 'xclip -selection clipboard'

### Running a test under cursor

In cargo project, the `:RustTest` command will run a test under the cursor.
This is useful when your project is bigger and running all tests take longer time.

## Help

Further help can be found in the documentation with `:Helptags` then `:help rust`.

Detailed help can be found in the documentation with `:help rust`.
Helptags (`:help helptags`) need to be generated for this plugin
in order to navigate the help. Most plugin managers will do this
automatically, but check their documentation if that is not the case.

## License

Like Rust, rust.vim is primarily distributed under the terms of both the MIT
license and the Apache License (Version 2.0). See LICENSE-APACHE and
LICENSE-MIT for details.

[r]: https://www.rust-lang.org
[v]: https://github.com/gmarik/vundle
[vqs]: https://github.com/gmarik/vundle#quick-start
[p]: https://github.com/tpope/vim-pathogen
[nb]: https://github.com/Shougo/neobundle.vim
[vp]: https://github.com/junegunn/vim-plug
[d]: https://github.com/Shougo/dein.vim
[rfmt]: https://github.com/rust-lang-nursery/rustfmt
[syn]: https://github.com/scrooloose/syntastic
[tgbr]: https://github.com/majutsushi/tagbar
[uctags]: https://ctags.io
[wav]: https://github.com/mattn/webapi-vim
[pp]: https://play.rust-lang.org/
[vim8pack]: http://vimhelp.appspot.com/repeat.txt.html#packages
