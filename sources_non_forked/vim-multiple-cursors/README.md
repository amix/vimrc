# vim-multiple-cursors
[![Build Status](https://travis-ci.org/terryma/vim-multiple-cursors.svg?branch=master)](https://travis-ci.org/github/terryma/vim-multiple-cursors)

## Contents
 - [About](#about)
 - [Installation](#installation)
 - [Quick Start](#quick-start)
 - [Mapping](#mapping)
 - [Settings](#settings)
 - [Interactions with other plugins](#interactions-with-other-plugins)
 - [Highlight](#highlight)
 - [FAQ](#faq)
 - [Contributing](#contributing)
 - [Credit](#credit)

## About
[There](https://github.com/paradigm/vim-multicursor) [have](https://github.com/felixr/vim-multiedit) [been](https://github.com/hlissner/vim-multiedit) [many](https://github.com/adinapoli/vim-markmultiple) [attempts](https://github.com/AndrewRadev/multichange.vim) at bringing Sublime Text's awesome [multiple selection][sublime-multiple-selection] feature into Vim, but none so far have been in my opinion a faithful port that is simplistic to use, yet powerful and intuitive enough for an existing Vim user. [vim-multiple-cursors] is yet another attempt at that.

### It's great for quick refactoring
![Example1](assets/example1.gif?raw=true)

Vim command sequence: `fp<C-n><C-n><C-n>cname`

### Add a cursor to each line of your visual selection
![Example2](assets/example2.gif?raw=true)

Vim command sequence: `vip<C-n>i"<Right><Right><Right>",<Esc>vipgJ$r]Idays = [`

### Match characters from visual selection
![Example3](assets/example3.gif?raw=true)

Vim command sequence: `df[$r,0f,v<C-n>…<C-n>c<CR><Up><Del><Right><Right><Right><Del>`

### Use the command to match regexp
![Example4](assets/example4.gif?raw=true)

To see what keystrokes are used for the above examples, see [the wiki page](https://github.com/terryma/vim-multiple-cursors/wiki/Keystrokes-for-example-gifs).

## Installation
Install using [Pathogen], [Vundle], [Neobundle], [vim-plug], or your favorite Vim package manager.

Requires vim 7.4 or newer for full functionality.

### vim-plug instructions

1. Paste this block into the top of `~/.vimrc`.

```vim script
call plug#begin()

Plug 'terryma/vim-multiple-cursors'

call plug#end()
```

2. Start vim and execute `:PlugInstall`.

## Quick Start
### normal mode / visual mode
  * start:          `<C-n>` start multicursor and add a _virtual cursor + selection_ on the match
    * next:         `<C-n>` add a new _virtual cursor + selection_ on the next match
    * skip:         `<C-x>` skip the next match
    * prev:         `<C-p>` remove current _virtual cursor + selection_ and go back on previous match
  * select all:     `<A-n>` start multicursor and directly select all matches

You can now change the _virtual cursors + selection_ with **visual mode** commands.
For instance: `c`, `s`, `I`, `A` work without any issues.
You could also go to **normal mode** by pressing `v` and use normal commands there.

At any time, you can press `<Esc>` to exit back to regular Vim.

**NOTE**: start with `g<C-n>` to match without boundaries (behaves like `g*` instead of `*`)

### visual mode when multiple lines are selected
  * start: `<C-n>` add _virtual cursors_ on each line

You can now change the _virtual cursors_ with **normal mode** commands.
For instance: `ciw`.

### command
The command `MultipleCursorsFind` accepts a range and a pattern (regexp), it creates a _visual cursor_ at the end of each match.
If no range is passed in, then it defaults to the entire buffer.


## Mapping
If you don't like the plugin taking over your key bindings, you can turn it off and reassign them the way you want:
```viml
let g:multi_cursor_use_default_mapping=0

" Default mapping
let g:multi_cursor_start_word_key      = '<C-n>'
let g:multi_cursor_select_all_word_key = '<A-n>'
let g:multi_cursor_start_key           = 'g<C-n>'
let g:multi_cursor_select_all_key      = 'g<A-n>'
let g:multi_cursor_next_key            = '<C-n>'
let g:multi_cursor_prev_key            = '<C-p>'
let g:multi_cursor_skip_key            = '<C-x>'
let g:multi_cursor_quit_key            = '<Esc>'
```

**NOTE:** Please make sure to always map something to `g:multi_cursor_quit_key`, otherwise you'll have a tough time quitting from multicursor mode.

## Settings
Currently there are four additional global settings one can tweak:

### ```g:multi_cursor_support_imap``` (Default: 1)
If set to 0, insert mappings won't be supported in _Insert_ mode anymore.

### ```g:multi_cursor_exit_from_visual_mode``` (Default: 0)
If set to 1, then pressing `g:multi_cursor_quit_key` in _Visual_ mode will quit and
delete all existing cursors, just skipping normal mode with multiple cursors.

### ```g:multi_cursor_exit_from_insert_mode``` (Default: 0)
If set to 1, then pressing `g:multi_cursor_quit_key` in _Insert_ mode will quit and
delete all existing cursors, just skipping normal mode with multiple cursors.

### ```g:multi_cursor_normal_maps``` (Default: see below)
`{'@': 1, 'F': 1, 'T': 1, '[': 1, '\': 1, ']': 1, '!': 1, '"': 1, 'c': 1, 'd': 1, 'f': 1, 'g': 1, 'm': 1, 'q': 1, 'r': 1, 't': 1, 'y': 1, 'z': 1, '<': 1, '=': 1, '>': 1}`

Any key in this map (values are ignored) will cause multi-cursor _Normal_ mode
to pause for map completion just like normal vim. Otherwise keys mapped in
normal mode will "fail to replay" when multiple cursors are active.
For example: `{'d':1}` makes normal-mode command `dw` work in multi-cursor mode.

The default list contents should work for anybody, unless they have remapped a
key from an operator-pending command to a non-operator-pending command or
vice versa.

These keys must be manually listed because vim doesn't provide a way to
automatically see which keys _start_ mappings, and trying to run motion commands
such as `j` as if they were operator-pending commands can break things.

### ```g:multi_cursor_visual_maps``` (Default: see below)
`{'T': 1, 'a': 1, 't': 1, 'F': 1, 'f': 1, 'i': 1}`

Same principle as `g:multi_cursor_normal_maps`

### Interactions with other plugins

### ```Multiple_cursors_before/Multiple_cursors_after``` (Default: `nothing`)

Other plugins may be incompatible in insert mode.
That is why we provide hooks to disable those plug-ins when vim-multiple-cursors is active:

For example, if you are using [Neocomplete](https://github.com/Shougo/neocomplete.vim),
add this to your vimrc to prevent conflict:

```viml
function! Multiple_cursors_before()
  if exists(':NeoCompleteLock')==2
    exe 'NeoCompleteLock'
  endif
endfunction

function! Multiple_cursors_after()
  if exists(':NeoCompleteUnlock')==2
    exe 'NeoCompleteUnlock'
  endif
endfunction
```

Plugins themselves can register `User` autocommands on `MultipleCursorsPre` and
`MultipleCursorsPost` for automatic integration.

### Highlight
The plugin uses the highlight group `multiple_cursors_cursor` and `multiple_cursors_visual` to highlight the virtual cursors and their visual selections respectively. You can customize them by putting something similar like the following in your vimrc:

```viml
" Default highlighting (see help :highlight and help :highlight-link)
highlight multiple_cursors_cursor term=reverse cterm=reverse gui=reverse
highlight link multiple_cursors_visual Visual
```

## FAQ

#### **Q**  Pressing <kbd>i</kbd> after selecting words with <kbd>C-n</kbd> makes the plugin hang, why?
**A** When selecting words with <kbd>C-n</kbd>, the plugin behaves like in **visual** mode.
Once you pressed <kbd>i</kbd>, you can still press <kbd>I</kbd> to insert text.

#### **Q** <kbd>ALT</kbd>+<kbd>n</kbd> doesn't seem to work in VIM but works in gVIM, why?
**A** This is a well known terminal/Vim [issue](http://vim.wikia.com/wiki/Get_Alt_key_to_work_in_terminal), different terminal have different ways to send ```Alt+key```.
Try adding this in your `.vimrc` and **make sure to replace the string**:
```vim
if !has('gui_running')
  map "in Insert mode, type Ctrl+v Alt+n here" <A-n>
endif
```
Or remap the following:
```vim
g:multi_cursor_start_key
g:multi_cursor_select_all_key
```

#### **Q** <kbd>CTRL</kbd>+<kbd>n</kbd> doesn't seem to work in gVIM?
**A** Try setting `set selection=inclusive` in your `~/.gvimrc`

**A** Alternatively, you can just temporarily disable _exclusive_ selection whenever the plugin is active:
```VimL
augroup MultipleCursorsSelectionFix
    autocmd User MultipleCursorsPre  if &selection ==# 'exclusive' | let g:multi_cursor_save_selection = &selection | set selection=inclusive | endif
    autocmd User MultipleCursorsPost if exists('g:multi_cursor_save_selection') | let &selection = g:multi_cursor_save_selection | unlet g:multi_cursor_save_selection | endif
augroup END
```

### **Q** deoplete insert giberrish, how to fix this?
**A** use the `Multiple_cursors` functions, add this in your vimrc:

```VimL
    func! Multiple_cursors_before()
      if deoplete#is_enabled()
        call deoplete#disable()
        let g:deoplete_is_enable_before_multi_cursors = 1
      else
        let g:deoplete_is_enable_before_multi_cursors = 0
      endif
    endfunc
    func! Multiple_cursors_after()
      if g:deoplete_is_enable_before_multi_cursors
        call deoplete#enable()
      endif
    endfunc
```

#### **Q** is it also working on Mac?
**A** On Mac OS, [MacVim](https://code.google.com/p/macvim/) is known to work.

#### **Q** How can I select `n` keywords with several keystrokes? `200<C-n>` does not work.
**A** You can use :MultipleCursorsFind keyword. I have this binding in my vimrc:

```VimL
nnoremap <silent> <M-j> :MultipleCursorsFind <C-R>/<CR>
vnoremap <silent> <M-j> :MultipleCursorsFind <C-R>/<CR>
```

This allows one to search for the keyword using `*` and turn search results into cursors with `Alt-j`.


## Contributing
Patches and suggestions are always welcome! A list of open feature requests can be found [here](https://github.com/terryma/vim-multiple-cursors/labels/pull%20request%20welcome).

### Issue Creation
Contributor's time is precious and limited. Please ensure it meets the requirements outlined in [CONTRIBUTING.md](CONTRIBUTING.md).

### Pull Requests
Running the test suite requires ruby and rake as well as vim of course. Before submitting PR, please ensure the checks are passing:
```bash
cd vim-multiple-cursors/spec/
bundle exec rake
```

### Contributors
This is a community supported project. Here is the list of all the [Contributors](https://github.com/terryma/vim-multiple-cursors/graphs/contributors)

## Credit
Obviously inspired by Sublime Text's [multiple selection][sublime-multiple-selection] feature, also encouraged by Emac's [multiple cursors][emacs-multiple-cursors] implementation by Magnar Sveen

[vim-multiple-cursors]:http://github.com/terryma/vim-multiple-cursors
[sublime-multiple-selection]:http://www.sublimetext.com/docs/2/multiple_selection_with_the_keyboard.html
[Pathogen]:http://github.com/tpope/vim-pathogen
[Vundle]:http://github.com/gmarik/vundle
[Neobundle]:http://github.com/Shougo/neobundle.vim
[vim-plug]:https://github.com/junegunn/vim-plug
[emacs-multiple-cursors]:https://github.com/magnars/multiple-cursors.el
