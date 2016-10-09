System Copy
===========

System copy provides vim mappings for copying / pasting text to the os specific
clipboard.  Most people will be happy just setting their Vim clipboard to the
system clipboard, but I find that doing so pollutes my clipboard history.
Instead, this plugin creates a unique mapping that explicitly pulls content
from Vim into the system clipboard.

Usage
-----

System copy provides a mapping to copy to the system clipboard using a motion
or visual selection. It also provides a mapping for pasting from the system
clipboard.

The default mapping is `cp`, and can be followed by any motion or text
object. For instance:

- `cpiw` => copy word into system clipboard
- `cpi'` => copy inside single quotes to system clipboard

In addition, `cP` is mapped to copy the current line directly.

The sequence `cv` is mapped to paste the content of system clipboard to the
next line.

Clipboard Utilities
-------------------

 - OSX     - `pbcopy` and `pbpaste`
 - Windows - `clip` and `paste`
 - Linux   - `xsel`

Options
-------

`system-copy` uses default copy and paste command based on your OS, but
you can override either of these commands if you have more specific needs.

To declare custom copy command use following example:
``` vim
let g:system_copy#copy_command='xclip -sel clipboard'
```
And to declare custom paste command use:
``` vim
let g:system_copy#paste_command='xclip -sel clipboard -o'
```

Installation
------------

If you don't have a preferred installation method, I recommend using [Vundle](https://github.com/VundleVim/Vundle.vim).
Assuming you have Vundle installed and configured, the following steps will
install the plugin:

Add the following line to your `~/.vimrc` and then run `:PluginInstall` from
within Vim:

``` vim
call vundle#begin()
" ...
Plugin 'christoomey/vim-system-copy'
" ...
call vundle#end()
```
