A code minimap for Vim
======================

The Sublime text-editor can display an useful overview of the code as a
*minimap* sidebar.

We can implement the same thing in Vim, relying on the [Drawille
library](https://github.com/asciimoo/drawille) to 'draw' in text mode.

![minimap in action](http://picdrop.t3lab.com/qqpdtsbTow.gif)

This code is made available under a MIT license. See [LICENSE](LICENSE) for
details.

Features
--------

- displays the minimap of the currently active buffer (and updates when
  switching to a different buffer)
- synchronized scrolling
- live update while typing

Installation
------------

Note that this extension requires Vim with Python support.

### Vundle

With [vundle](https://github.com/gmarik/Vundle.vim), simply add: `Plugin
'severin-lemaignan/vim-minimap'` to your `.vimrc` and run `:PluginInstall` from
vim.

### Janus

With Janus just clone inside ```.janus```.

```
cd ~/.janus
git clone https://github.com/severin-lemaignan/vim-minimap.git vim-minimap
```

Usage
-----

`:Minimap` to show the minimap, `:MinimapClose` to hide it.

Default mappings: `<Leader>mm` to display the minimap, `<Leader>mc` to close it.

To overwrite the default keybindings, using following settings in ``.vimrc'':

```
let g:minimap_show='<leader>ms'
let g:minimap_update='<leader>mu'
let g:minimap_close='<leader>gc'
let g:minimap_toggle='<leader>gt'
```

Settings
--------

You can customize the color of the highlighting by setting `g:minimap_highlight` in your vimrc:

`let g:minimap_highlight='Visual'`

Note: To find out which highlights are available on your vim installation use :hi to get the list.

Troubleshooting
---------------

- Weird display

Certain fonts do not display plain dots and empty spaces, but
plain dots and circles for braille characters. As a result, you may want to use
any other font that display braille characters in a way that suit the minimap
plugin, like `Ubuntu Mono`, or `Droid Sans Mono`.

For example, with `Inconsolata`:
![image](https://cloud.githubusercontent.com/assets/7250745/8083430/c48e5c44-0f84-11e5-9cba-20d7e2eac0c5.png)

With `Ubuntu Mono`
![image](https://cloud.githubusercontent.com/assets/7250745/8083436/d4aaf9d4-0f84-11e5-9383-cb02bba384bc.png)

