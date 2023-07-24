### [Vim](http://www.vim.org/)

#### Install

These are the default instructions using Vim 8's `|packages|` feature. See
sections below, if you use other plugin managers.

1. Create theme folder (in case you don't have it yet):


- \*nix:
```
# vim 8.2+
mkdir -p ~/.vim/pack/themes/start
# vim 8.0
mkdir -p ~/.vim/pack/themes/opt
```

- Windows: create directory `$HOME\vimfiles\pack\themes\start` or
  `$HOME\vimfiles\pack\themes\opt`, according to your version.

2. Navigate to the folder above:


- \*nix:
```
# vim 8.2+
cd ~/.vim/pack/themes/start
# vim 8.0
cd ~/.vim/pack/themes/opt
```

- Windows: navigate to the directory you created earlier

3. Clone the repository using the "dracula" name:

```
git clone https://github.com/dracula/vim.git dracula
```
(Or use your favorite GUI client, or download the ZIP)

4. Edit your `vimrc` file with the following content:

```
if v:version < 802
    packadd! dracula
endif
syntax enable
colorscheme dracula
```

The location of the `vimrc` varies between platforms:
- \*nix: `~/.vim/vimrc` or `~/.vimrc`
- Windows: `$HOME\vimfiles\vimrc` or `$HOME\_vimrc`

#### Install using other plugin managers

- If you [use vim + pathogen + submodules](http://vimcasts.org/episodes/synchronizing-plugins-with-git-submodules-and-pathogen/):

Navigate to your vim directory (\*nix: `~/.vim`; Windows: `$HOME\vimfiles`)

    git submodule add git@github.com:dracula/vim.git bundle/dracula

Place `colorscheme dracula` after `execute pathogen#infect()`.

- If you [use vim + vundle](https://github.com/VundleVim/Vundle):

```vim
Plugin 'dracula/vim', { 'name': 'dracula' }
:PluginInstall
```

Place `colorscheme dracula` after `call vundle#end()`.

- If you [use vim-plug](https://github.com/junegunn/vim-plug) (\`as\` will install
the plugin in a directory called 'dracula' instead of just 'vim'):

```vim
Plug 'dracula/vim', { 'as': 'dracula' }
:PlugInstall
```

Place `colorscheme dracula` after `call plug#end()`.

- If you [use spacevim](https://spacevim.org), put the
following in `~/.SpaceVim.d/init.toml`:

```toml
[options]
  colorscheme = "dracula"
  colorscheme_bg = "dark"
[[custom_plugins]]
  repo = "dracula/vim"
  name = "dracula"
  merged = false
```

---

Note that dracula must be in your `'runtimepath'` to load properly: Version 2.0
introduced autoload functionality for part of the plugin, which doesn't work
without `'runtimepath'` properly set. Consult your plugin-managers documentation
to make sure you put dracula on the `'runtimepath'` before loading it. For
`|packages|`, versions 8.2 and later will autoload `start` packages
correctly even in your vimrc.
