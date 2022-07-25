### [Vim](http://www.vim.org/)

#### Install

These are the default instructions using Vim 8's `|packages|` feature. See
sections below, if you use other plugin managers.

1. Create theme folder (in case you don't have yet):


- \*nix:
```
mkdir -p ~/.vim/pack/themes/start
```

- Windows: create directory `$HOME\vimfiles\pack\themes\start`

If you use vim 8.0 (and not 8.2), you may need to use `~/.vim/pack/themes/opt`
or `$HOME\vimfiles\pack\themes\opt` instead.

2. Navigate to the folder above:


- \*nix:
```
cd ~/.vim/pack/themes/start
```

- Windows: navigate to `$HOME\vimfiles\pack\themes\start`

3. Clone the repository using the "dracula" name:

```
git clone https://github.com/dracula/vim.git dracula
```
(Or use your favorite GUI client, or download the ZIP)

4. Edit your `vimrc` file with the following content:

```
packadd! dracula
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
to make sure you put dracula on the `'runtimepath'` before loading it.
