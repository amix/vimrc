Node.vim
========
[![Build status](https://travis-ci.org/moll/vim-node.png?branch=master)](https://travis-ci.org/moll/vim-node)

Tools to make Vim superb for developing with Node.js.  
It's the Node equivalent of [Rails.vim (vimscript #1567)](https://github.com/tpope/vim-rails) and [Rake.vim (vimscript #3669)](https://github.com/tpope/vim-rake).

This is just the first release to get the nodes rolling. If you've collected great helpers and shortcuts that help you work with Node, please share them via [email](mailto:andri@dot.ee), [Twitter](https://twitter.com/theml) or [GitHub issues](https://github.com/moll/vim-node/issues) so we could incorporate them here, too! Thanks!

### Tour

- Use `gf` on paths or requires to open the same file Node.js would.
- Use `gf` on `require(".")` to open `./index.js`
- Use `gf` on `require("./dir")` to open `./dir/index.js`
- Use `gf` on `require("./foo")` to open `foo.js`.
- Use `gf` on `require("./package")` and have it open package.json.
- Use `gf` on `require("module")` to open the module's main file (parsed for you from `package.json`).
- Use `gf` on `require("module/lib/utils")` and open files inside the module.
- Automatically sets the filetype to JavaScript for files with Node's shebang (`#!`).
- Use `[I` etc. to look for a keyword in required files (Sets Vim's `&include`).
- Use `:Nedit` to quickly edit any module, file in a module or your project file.
- Use `:Nopen` to quickly edit any module and `lcd` to its directory.
- Lets you even open Node's core modules. They're shown straight from Node's online repository without you having to download everything.
- Node.vim itself is tested with a thorough automated integration test suite! No cowboy coding here!

Expect more to come soon and feel free to let me know what you're after!

PS. Node.vim is absolutely intended to work on Windows, but not yet tested there at all. If you could help, try it out and report issues, I'd be grateful!


Installing
----------
The easiest and most modular way is to download this to `~/.vim/bundle`:
```
mkdir -p ~/.vim/bundle/node
```

Using Git:
```
git clone https://github.com/moll/vim-node.git ~/.vim/bundle/node
```

Using Wget:
```
wget https://github.com/moll/vim-node/archive/master.tar.gz -O- | tar -xf- --strip-components 1 -C ~/.vim/bundle/node
```

Then prepend that directory to Vim's `&runtimepath` (or use [Pathogen](https://github.com/tpope/vim-pathogen)):
```
:set runtimepath^=~/.vim/bundle/node
```

### Vundle

Or use [Vundle](https://github.com/gmarik/Vundle.vim):
```
:BundleInstall moll/vim-node
```


Using
-----
Open any JavaScript file inside a Node project and you're all set.

- Use `gf` inside `require("...")` to jump to source and module files.
- Use `[I` on any keyword to look for it in the current and required files.
- Use `:Nedit module_name` to edit the main file of a module.
- Use `:Nedit module_name/lib/foo` to edit its `lib/foo.js` file.
- Use `:Nedit .` to edit your Node projects main (usually `index.js`) file.

#### Want to customize settings for files inside a Node projects?
Use the `Node` autocommand. For example:
```vim
autocmd User Node if &filetype == "javascript" | setlocal expandtab | endif
```

#### Want `<C-w>f` to open the file under the cursor in a new vertical split?
`<C-w>f` by default opens it in a horizontal split. To have it open vertically, drop this in your `vimrc`:
```vim
autocmd User Node
  \ if &filetype == "javascript" |
  \   nmap <buffer> <C-w>f <Plug>NodeVSplitGotoFile |
  \   nmap <buffer> <C-w><C-f> <Plug>NodeVSplitGotoFile |
  \ endif
```

License
-------
Node.vim is released under a *Lesser GNU Affero General Public License*, which in summary means:

- You **can** use this program for **no cost**.
- You **can** use this program for **both personal and commercial reasons**.
- You **do not have to share your own program's code** which uses this program.
- You **have to share modifications** (e.g bug-fixes) you've made to this program.

For more convoluted language, see the `LICENSE` file.


About
-----
**[Andri Möll](http://themoll.com)** authored this in SublemacslipseMate++.  
[Monday Calendar](https://mondayapp.com) supported the engineering work.  

If you find Node.vim needs improving or you've got a question, please don't hesitate to email me anytime at [andri@dot.ee](mailto:andri@dot.ee), tweet at [@theml](https://twitter.com/theml) or [create an issue online](https://github.com/moll/vim-node/issues).
