# lightline.vim
A light and configurable statusline/tabline plugin for Vim

https://github.com/itchyny/lightline.vim

### powerline (default)

![lightline.vim - powerline](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/powerline.png)

### wombat

![lightline.vim - wombat](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/wombat.png)

### jellybeans

![lightline.vim - jellybeans](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/jellybeans.png)

### solarized dark

![lightline.vim - solarized_dark](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/solarized_dark.png)

### solarized light

![lightline.vim - solarized_light](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/solarized_light.png)

### PaperColor light

![lightline.vim - PaperColor](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/PaperColor.png)

### seoul256

![lightline.vim - seoul256](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/seoul256.png)

### one

![lightline.vim - one](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/one.png)

### landscape

![lightline.vim - landscape](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/landscape.png)

landscape is my colorscheme, which is a high-contrast cterm-supported colorscheme, available at https://github.com/itchyny/landscape.vim

## Why yet another clone of powerline?
+ [vim-powerline](https://github.com/Lokaltog/vim-powerline) is a nice plugin, but deprecated.
+ [powerline](https://github.com/powerline/powerline) is a nice plugin, but difficult to configure.
+ [vim-airline](https://github.com/vim-airline/vim-airline) is a nice plugin, but it uses too much functions of other plugins, which should be done by users in `.vimrc`.

## Spirit of this plugin
+ Minimalism. The core script is very small to achieve enough functions as a statusline plugin.
+ Configurability. You can create your own component and easily add to the statusline and the tabline.
+ Orthogonality. The plugin does not rely on the implementation of other plugins. Such plugin crossing settings should be configured by users.

## Installation
### [Pathogen](https://github.com/tpope/vim-pathogen)
1. Install with the following command.

        git clone https://github.com/itchyny/lightline.vim ~/.vim/bundle/lightline.vim

### [Vundle](https://github.com/VundleVim/Vundle.vim)
1. Add the following configuration to your `.vimrc`.

        Plugin 'itchyny/lightline.vim'

2. Install with `:PluginInstall`.

### [NeoBundle](https://github.com/Shougo/neobundle.vim)
1. Add the following configuration to your `.vimrc`.

        NeoBundle 'itchyny/lightline.vim'

2. Install with `:NeoBundleInstall`.

### [vim-plug](https://github.com/junegunn/vim-plug)
1. Add the following configuration to your `.vimrc`.

        Plug 'itchyny/lightline.vim'

2. Install with `:PlugInstall`.

## Introduction
After installing this plugin, you restart the editor and will get a cool statusline.
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/1.png)

The color of the statusline changes due to the mode of Vim. Try typing something, selecting in visual mode and replacing some texts.

If the statusline looks like
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/21.png)

add the following configuration to your `.vimrc`.
```vim
set laststatus=2
```

If the statusline is not coloured like
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/20.png)

then modify `TERM` in your shell configuration (`.zshrc` for example)
```sh
export TERM=xterm-256color
```
and then add the following configure to your `.vimrc`.
```vim
if !has('gui_running')
  set t_Co=256
endif
```

Your statusline appears to work correctly? If yes, great, thanks for choosing lightline.vim! If no, please file an issue report to the [issue tracker](https://github.com/itchyny/lightline.vim/issues).

By the way, `-- INSERT --` is unnecessary anymore because the mode information is displayed in the statusline.
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/13.png)
If you want to get rid of it, configure as follows.
```vim
set noshowmode
```

## Colorscheme configuration
The lightline.vim plugin provides multiple colorschemes to meet your editor colorscheme.
Do not be confused, editor colorscheme rules how codes look like in buffers and lightline.vim has independent colorscheme feature, which rules how the statusline looks like.

If you are using wombat colorscheme, add the following setting to your `.vimrc`,
```vim
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ }
```
restart Vim and the statusline looks like:

![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/2.png)

If the colors of the statusline do not change, move the settings of `g:lightline` before setting the editor colorscheme.

There are many lightline colorschemes available as screenshots shown above. See `:h g:lightline.colorscheme` for the complete list.

## Advanced configuration
The default appearance of lightline.vim is carefully designed that the tutorial is enough here for most people.
So please read this section if you really want to configure and enjoy the configurability of lightline.vim.

Sometimes people want to display information of other plugins.
For example git branch information, syntax check errors and some statuses of plugins.

The lightline.vim plugin does not provide any plugin integration by default.
This plugin considers orthogonality to be one of the important ideas, which means that the plugin does not rely on implementation of other plugins.
Once a plugin starts to integrate with some famous plugins, it should be kept updated to follow the changes of the plugins, and should accept integration requests with new plugins and it will suffer from performance regression due to plugin availability checks.

Instead, lightline.vim provides a simple API that user can easily integrate with other plugins.
Once you understand how to configure and how it will be displayed in the statusline, you can also tell how to integrate with your favorite plugins.

Let's start to configure the appearance.
The statusline is composed by multiple components.
It shows the current mode, filename, modified status on the left, and file format, encoding, filetype and cursor positions on the right.
So in order to add something in the statusline, you firstly create a new component and specify the place.

This is the hello world of lightline.vim component.
```vim
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified', 'helloworld' ] ]
      \ },
      \ 'component': {
      \   'helloworld': 'Hello, world!'
      \ },
      \ }
```
The statusline will look like:
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/3.png)

You have succeeded in displaying `Hello, world!` in the statusline.
The `helloworld` component is added to `g:lightline.active.left` and its content is configured in `g:lightline.component`.
The component contents are simply added to `&statusline`.
Try `:echo &statusline`, it might be a little bit complicated, but you will find `Hello, world!` somewhere.

You can use `'statusline'` syntax for lightline.vim components.
Consult `:h 'statusline'` to see what's available here.
For example, if you want to print the value of character under the cursor in hexadecimal, configure as
```vim
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified', 'charvaluehex' ] ]
      \ },
      \ 'component': {
      \   'charvaluehex': '0x%B'
      \ },
      \ }
```
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/4.png)

You want the character value information on the right hand side? OK, configure as
```vim
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'fileformat', 'fileencoding', 'filetype', 'charvaluehex' ] ]
      \ },
      \ 'component': {
      \   'charvaluehex': '0x%B'
      \ },
      \ }
```
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/5.png)

We have learned how to add a simple component.

- See `:h 'statusline'` to check the statusline flags.
- Add a new component to `g:lightline.component`.
- Add the component name to `g:lightline.active.left` or `g:lightline.active.right`.

You can also configure the statusline of inactive buffers by adding the component to `g:lightline.inactive.left` or `g:lightline.inactive.right`.


Now let's add some integrations with other plugin.
The name of the git branch is important these days.
But lightline.vim does not provide this information by default because it is also one of plugin crossing configurations, and not all people want the integration.

In order to show the branch name in the statusline, install some plugins which provides the branch information.
The [vim-fugitive](https://github.com/tpope/vim-fugitive) plugin is a famous plugin so let's integrate lightline.vim with it.
If you don't like to install full git integration but just want to display the branch name in the statusline, you can use the [vim-gitbranch](https://github.com/itchyny/vim-gitbranch) plugin which provides `gitbranch#name` function.
```vim
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head'
      \ },
      \ }
```
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/6.png)

Okay, now the statusline shows that we are coding at the master branch.
What do we learn from this example?

- Find out the function which is suitable to use in the statusline.
- Create a function component. The previous `charvaluehex` component has `'statusline'` item configuration and registered in `g:lightline.component`. In the current example, we register the name of the function in `g:lightline.component_function`. It should return the string to be displayed in the statusline.
- Add the component name `gitbranch` to `g:lightline.active.left` or `g:lightline.active.right`.


Here we have leaned two kinds of components.

- component: it has a `%`-prefixed item which you can find the meaning at `:h 'statusline'`. All the default components of lightline.vim are components in this style. See the default components at `:h g:lightline.component`.
- function component: the name of functions are registered. The function is called again and again so be careful not to register a heavy function. See the help with `:h g:lightline.component_function`.


The function component is an important design for the configurability of lightline.vim.
By providing the configuration interface via functions, you can adjust the statusline information as you wish.
For the proof, let's look into some configuration examples in Q&amp;A style.

### Can I hide the readonly component in the help buffer?
Yes, create a function component for `readonly`.
The configuration of function component has priority over the default component.
```vim
let g:lightline = {
      \ 'component_function': {
      \   'readonly': 'LightlineReadonly',
      \ },
      \ }

function! LightlineReadonly()
  return &readonly && &filetype !=# 'help' ? 'RO' : ''
endfunction
```
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/7.png)

### Can I hide the readonly component in other plugins buffer?
Yes, modify the `LightlineReadonly` function as you wish.
```vim
function! LightlineReadonly()
  return &readonly && &filetype !~# '\v(help|vimfiler|unite)' ? 'RO' : ''
endfunction

let g:unite_force_overwrite_statusline = 0
let g:vimfiler_force_overwrite_statusline = 0
```
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/8.png)

### Can I display the plugin information at the filename component?
Yes, overwrite the filename component.
```vim
let g:lightline = {
      \ 'component_function': {
      \   'filename': 'LightlineFilename',
      \ },
      \ }

function! LightlineFilename()
  return &filetype ==# 'vimfiler' ? vimfiler#get_status_string() :
        \ &filetype ==# 'unite' ? unite#get_status_string() :
        \ &filetype ==# 'vimshell' ? vimshell#get_status_string() :
        \ expand('%:t') !=# '' ? expand('%:t') : '[No Name]'
endfunction

let g:unite_force_overwrite_statusline = 0
let g:vimfiler_force_overwrite_statusline = 0
let g:vimshell_force_overwrite_statusline = 0
```
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/9.png)

### Can I display the plugin name at the mode component?
Yes, overwrite the mode component.
```vim
let g:lightline = {
      \ 'component_function': {
      \   'mode': 'LightlineMode',
      \ },
      \ }

function! LightlineMode()
  return expand('%:t') ==# '__Tagbar__' ? 'Tagbar':
        \ expand('%:t') ==# 'ControlP' ? 'CtrlP' :
        \ &filetype ==# 'unite' ? 'Unite' :
        \ &filetype ==# 'vimfiler' ? 'VimFiler' :
        \ &filetype ==# 'vimshell' ? 'VimShell' :
        \ lightline#mode()
endfunction
```
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/10.png)

### Can I trim the file format and encoding information on narrow windows?
Yes, check `winwidth(0)` and return empty string with some threshold.
```vim
let g:lightline = {
      \ 'component_function': {
      \   'fileformat': 'LightlineFileformat',
      \   'filetype': 'LightlineFiletype',
      \ },
      \ }

function! LightlineFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightlineFiletype()
  return winwidth(0) > 70 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction
```
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/11.png)

### Can I trim the bar between the filename and modified sign?
Yes, by joining the two components.
```vim
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename' ] ],
      \ },
      \ 'component_function': {
      \   'filename': 'LightlineFilename',
      \ },
      \ }

function! LightlineFilename()
  let filename = expand('%:t') !=# '' ? expand('%:t') : '[No Name]'
  let modified = &modified ? ' +' : ''
  return filename . modified
endfunction
```
![lightline.vim - tutorial](https://raw.githubusercontent.com/wiki/itchyny/lightline.vim/image/tutorial/12.png)

You can control the visibility and contents by writing simple functions.
Now you notice how much function component is important for the configurability of lightline.vim.

## Note for developers of other plugins
Appearance consistency matters.

The statusline is an important space for Vim users.
Overwriting the statusline forcibly in your plugin is not a good idea.
It is not hospitality, but just an annoying feature.
If your plugin has such a feature, add an option to be modest.

A good design is as follows.
Firstly, give the users a clue to judge which buffer is the one your plugin creates.
The filename is a manner and the filetype is another.
Then, export a function which is useful to be shown in the statusline.
Lastly, for advanced users, set important information in buffer variables so that the users can obtain the condition of the plugin easily.

## Author
itchyny (https://github.com/itchyny)

## License
This software is released under the MIT License, see LICENSE.
