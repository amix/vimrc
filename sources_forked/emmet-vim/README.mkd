# Emmet-vim

[emmet-vim](http://mattn.github.com/emmet-vim) is a vim plug-in
which provides support for expanding abbreviations similar to
[emmet](http://emmet.io/).

[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/mattn/emmet-vim/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

![](https://raw.githubusercontent.com/mattn/emmet-vim/master/doc/screenshot.gif)

## Installation

[Download zip file](http://www.vim.org/scripts/script.php?script_id=2981):

    cd ~/.vim
    unzip emmet-vim.zip

To install using pathogen.vim:

    cd ~/.vim/bundle
    git clone https://github.com/mattn/emmet-vim.git
    
To install using [Vundle](https://github.com/gmarik/vundle):

    " add this line to your .vimrc file
    Plugin 'mattn/emmet-vim'

To checkout the source from repository:

    cd ~/.vim/bundle
    git clone https://github.com/mattn/emmet-vim.git

or:

    git clone https://github.com/mattn/emmet-vim.git
    cd emmet-vim
    cp plugin/emmet.vim ~/.vim/plugin/
    cp autoload/emmet.vim ~/.vim/autoload/
    cp -a autoload/emmet ~/.vim/autoload/


## Quick Tutorial

Open or create a New File:

    vim index.html

Type ("\_" is the cursor position):

    html:5_

Then type `<c-y>,` (<kbd>Ctrl</kbd><kbd>y</kbd><kbd>,</kbd>), and you should see:

```html
<!DOCTYPE HTML>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<title></title>
</head>
<body>
	_
</body>
</html>
```

[More Tutorials](https://raw.github.com/mattn/emmet-vim/master/TUTORIAL)


## Enable in different mode

If you don't want to enable emmet in all modes,
you can use set these options in `vimrc`:

```vim
let g:user_emmet_mode='n'    "only enable normal mode functions.
let g:user_emmet_mode='inv'  "enable all functions, which is equal to
let g:user_emmet_mode='a'    "enable all function in all mode.
```

## Enable just for html/css

```vim
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall
```

## Redefine trigger key
To remap the default `<C-Y>` leader:

```vim
let g:user_emmet_leader_key='<C-Z>'
```

Note that the trailing `,` still needs to be entered, so the new keymap would be `<C-Z>,`.

## Adding custom snippets
If you have installed the [web-api](https://github.com/mattn/webapi-vim) for **emmet-vim** you can also add your own snippets using a custom **snippets.json** file.

Once you have installed the [web-api](https://github.com/mattn/webapi-vim) add this line to your **.vimrc**:
```
let g:user_emmet_settings = webapi#json#decode(join(readfile(expand('~/.snippets_custom.json')), "\n"))
```
You can change the **path** to your **snippets_custom.json** according to your preferences.

[Here](http://docs.emmet.io/customization/snippets/) you can find instructions about creating your customized **snippets.json** file.

## Project Authors

[Yasuhiro Matsumoto](http://mattn.kaoriya.net/)

## Links

### Emmet official site:

* <http://emmet.io/>

### zen-coding official site:

* <http://code.google.com/p/zen-coding/>

### emmet.vim:

* <http://mattn.github.com/emmet-vim>

### development repository:

* <https://github.com/mattn/emmet-vim>

### my blog posts about zencoding-vim:

* <http://mattn.kaoriya.net/software/vim/20100222103327.htm>

* <http://mattn.kaoriya.net/software/vim/20100306021632.htm>

### Japanese blog posts about zencoding-vim:

* <http://d.hatena.ne.jp/idesaku/20100424/1272092255>

* <http://d.hatena.ne.jp/griefworker/20110118/vim_zen_coding>

* <http://d.hatena.ne.jp/sakurako_s/20110126/1295988873>

* <http://looxu.blogspot.jp/2010/02/zencodingvimhtml.html>

### A Chinese translation of the tutorial:

* <http://www.zfanw.com/blog/zencoding-vim-tutorial-chinese.html>

