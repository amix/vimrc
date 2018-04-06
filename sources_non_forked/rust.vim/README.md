# rust.vim

## Description

This is a Vim plugin that provides [Rust][r] file detection, syntax highlighting, formatting,
[Syntastic][syn] integration, and more.

## Installation

### Using [Vundle][v]

1. Add `Plugin 'rust-lang/rust.vim'` to `~/.vimrc`
2. `:PluginInstall` or `$ vim +PluginInstall +qall`

*Note:* Vundle will not automatically detect Rust files properly if `filetype
on` is executed before Vundle. Please check the [quickstart][vqs] for more
details.

### Using [Pathogen][p]

```shell
git clone --depth=1 https://github.com/rust-lang/rust.vim.git ~/.vim/bundle/rust.vim
```

### Using [NeoBundle][nb]

1. Add `NeoBundle 'rust-lang/rust.vim'` to `~/.vimrc`
2. Re-open vim or execute `:source ~/.vimrc`

### Using [vim-plug][vp]

1. Add `Plug 'rust-lang/rust.vim'` to `~/.vimrc`
2. `:PlugInstall` or `$ vim +PlugInstall +qall`

## Features

### Error checking with [Syntastic][syn]

`rust.vim` automatically registers `rustc` as a syntax checker
with [Syntastic][syn]. Check Syntastic's documentation for
information on how to customize its behaviour.

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

[rfmt]: https://crates.io/crates/rustfmt/

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
[rfmt]: https://github.com/rust-lang-nursery/rustfmt
[syn]: https://github.com/scrooloose/syntastic
[wav]: https://github.com/mattn/webapi-vim
[pp]: https://play.rust-lang.org/
