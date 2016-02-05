# Vim Racer Plugin

This plugin allows vim to use [Racer](http://github.com/phildawes/racer) for Rust code completion and navigation.

## Installation

1. Build / Install [Racer](http://github.com/phildawes/racer)

1. Install using Pathogen, Vundle or NeoBundle. Or, copy `plugin/racer.vim` into your `~/.vim/plugin` directory.

  Vundle users:
  ```
  Plugin 'racer-rust/vim-racer'
  ```

  NeoBundle users:
  ```
  NeoBundle 'racer-rust/vim-racer'
  ```
  
  vim-plug users:
  ```
  Plug 'racer-rust/vim-racer'
  ```

2. Add `g:racer_cmd` and `$RUST_SRC_PATH` variables to your `.vimrc`. Also it's worth turning on 'hidden' mode for buffers otherwise you need to save the current buffer every time you do a goto-definition. E.g.:

     ```
     set hidden
     let g:racer_cmd = "<path-to-racer>/target/release/racer"
     let $RUST_SRC_PATH="<path-to-rust-srcdir>/src/"
     ```

3. In insert mode use `C-x-C-o` to search for completions

4. In normal mode type `gd` to go to a definition
