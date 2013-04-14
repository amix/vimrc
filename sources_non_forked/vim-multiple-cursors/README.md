# vim-multiple-cursors

## About
[There](https://github.com/paradigm/vim-multicursor) [have](https://github.com/felixr/vim-multiedit) [been](https://github.com/hlissner/vim-multiedit) [many](https://github.com/adinapoli/vim-markmultiple) [attempts](https://github.com/AndrewRadev/multichange.vim) at bringing Sublime Text's awesome [multiple selection][sublime-multiple-selection] feature into Vim, but none so far have been in my opinion a faithful port that is simplistic to use, yet powerful and intuitive enough for an existing Vim user. [vim-multiple-cursors] is yet another attempt at that.

### It's great for quick refactoring
![Example1](assets/example1.gif?raw=true)

### Add a cursor to each line of your visual selection
![Example2](assets/example2.gif?raw=true)

### Do it backwards too! This is not just a replay of the above gif :)
![Example3](assets/example3.gif?raw=true)

## Features
- Live update in Insert mode
- One key to rule it all! See [Quick Start](#quick-start) on what the key does in different scenarios
- Works in Normal, Insert, and Visual mode for SINGLE key command

## Installation
Install using [Pathogen], [Vundle], [Neobundle], or your favorite Vim package manager.

## Quick Start
Out of the box, all you need to know is a single key `Ctrl-n`. Pressing the key in Normal mode highlights the current word under the cursor in Visual mode and places a virtual cursor at the end of it. Pressing it again finds the next ocurrence and places another virtual cursor at the end of the visual selection. If you select multiple lines in Visual mode, pressing the key puts a virtual cursor at every line and leaves you in Normal mode.

After you've marked all your locations with `Ctrl-n`, you can change the visual selection with normal Vim motion commands in Visual mode. You could go to Normal mode by pressing `v` and wield your motion commands there. Single key command to switch to Insert mode such as `c` or `s` from Visual mode or `i`, `a`, `I`, `A` in Normal mode should work without any issues.

At any time, you can press `<Esc>` to exit back to regular Vim.

Two additional keys are also mapped:
- `Ctrl-p` in Visual mode will remove the current virtual cursor and go back to the previous virtual cursor location. This is useful if you are trigger happy with `Ctrl-n` and accidentally went too far.
- `Ctrl-x` in Visual mode will remove the current virtual cursor and skip to the next virtual cursor location. This is useful if you don't want the current selection to be a candidate to operate on later.

**NOTE**: The plugin is still somewhat buggy, if at any time you have lingering cursors on screen, you can press `Ctrl-n` in Normal mode and it will remove all prior cursors before starting a new one.

## Mapping
Out of the box, `Ctrl-n`, `Ctrl-p`, and `Ctrl-x` are mapped by default. If you don't like the plugin taking over your favorite key bindings, then turn off the default with
```
let g:multi_cursor_use_default_mapping=0
```

You can map the 'next', 'previous', 'skip', and 'exit' keys like the following:
```
" Default mapping
let g:multi_cursor_next_key="\<C-n>"
let g:multi_cursor_prev_key="\<C-p>"
let g:multi_cursor_skip_key="\<C-x>"
let g:multi_cursor_exit_key="\<Esc>"
```

## Setting
Currently there're two additional global settings one can tweak:
### ```g:multi_cursor_exit_from_visual_mode``` (Defaut: 1)

If set to 0, then pressing `g:multi_cursor_exit_key` in _Visual_ mode will not quit and delete all existing cursors. This is useful if you want to press Escape and go back to Normal mode, and still be able to operate on all the cursors.

### ```g:multi_cursor_exit_from_insert_mode``` (Default: 1)
If set to 0, then pressing `g:multi_cursor_exit_key` in _Insert_ mode will not quit and delete all existing cursors. This is useful if you want to press Escape and go back to Normal mode, and still be able to operate on all the cursors.

### Highlight
The plugin uses the highlight group `multiple_cursors_cursor` and `multiple_cursors_visual` to highlight the virtual cursors and their visual selections respectively. You can customize them by putting something similar like the following in your vimrc:

```
" Default highlighting (see help :highlight and help :highlight-link)
highlight multiple_cursors_cursor term=reverse cterm=reverse gui=reverse
highlight link multiple_cursors_visual Visual
```

## Issues
- Multi key commands like `ciw` do not work at the moment
- All user input typed before Vim is able to fan out the last operation to all cursors is lost. This is a implementation decision to keep the input perfectly synced in all locations, at the cost of potentially losing user input.
- Single key commands that do not terminate properly cause unexpected behavior. For example, if the cursor is on the first character in the buffer and 'b' is pressed.
- Undo behavior is unpredictable
- Performance in terminal vim degrades significantly with more cursors
- Select mode is not implemented
- Buggy when `wrap` is turned on
- Cursor highlighting is off. The last column on the same row as Vim's cursor is not highlighted incorrectly. Setting virtualedit=all might help

## Contributing
As one can see, there're still many issues to be resolved, patches and suggestions are always welcome!

## Credit
Obviously inspired by Sublime Text's [multiple selection][sublime-multiple-selection] feature, also encouraged by Emac's [multiple cursors][emacs-multiple-cursors] implemetation by Magnar Sveen

[vim-multiple-cursors]:http://github.com/terryma/vim-multiple-cursors
[sublime-multiple-selection]:http://www.sublimetext.com/docs/2/multiple_selection_with_the_keyboard.html
[Pathogen]:http://github.com/tpope/vim-pathogen
[Vundle]:http://github.com/gmarik/vundle
[Neobundle]:http://github.com/Shougo/neobundle.vim
[emacs-multiple-cursors]:https://github.com/magnars/multiple-cursors.el
