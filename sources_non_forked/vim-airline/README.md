# vim-airline [![Build Status](https://travis-ci.org/bling/vim-airline.png)](https://travis-ci.org/bling/vim-airline)

Lean &amp; mean status/tabline for vim that's light as air.

![img](https://github.com/bling/vim-airline/wiki/screenshots/demo.gif)

# Features

*  Tiny core written with extensibility in mind ([open/closed principle][8]).
*  Integrates with a variety of plugins, including: [vim-bufferline][6], [fugitive][4], [unite][9], [ctrlp][10], [minibufexpl][15], [gundo][16], [undotree][17], [nerdtree][18], [tagbar][19], [vim-gitgutter][29], [vim-signify][30], [syntastic][5], [eclim][34], [lawrencium][21] and [virtualenv][31].
*  Looks good with regular fonts and provides configuration points so you can use unicode or powerline symbols.
*  Optimized for speed; it loads in under a millisecond.
*  Extensive suite of themes for popular colorschemes including [solarized][23] (dark and light), [tomorrow][24] (all variants), [base16][32] (all variants), [molokai][25], [jellybeans][26] and others; have a look at the [screenshots][14] in the wiki.
*  Supports 7.2 as the minimum Vim version.
*  The master branch tries to be as stable as possible, and new features are merged in only after they have gone through a [full regression test][33].
*  Unit testing suite.

## Straightforward customization

If you don't like the defaults, you can replace all sections with standard `statusline` syntax.  Give your statusline that you've built over the years a face lift.

![image](https://f.cloud.github.com/assets/306502/1009429/d69306da-0b38-11e3-94bf-7c6e3eef41e9.png)

## Automatic truncation

Sections and parts within sections can be configured to automatically hide when the window size shrinks.

![image](https://f.cloud.github.com/assets/306502/1060831/05c08aac-11bc-11e3-8470-a506a3037f45.png)

## Smarter tab line

Automatically displays all buffers when there's only one tab open.

![tabline](https://f.cloud.github.com/assets/306502/1072623/44c292a0-1495-11e3-9ce6-dcada3f1c536.gif)

This is disabled by default; add the following to your vimrc to enable the extension:

    let g:airline#extensions#tabline#enabled = 1

Separators can be configured independently for the tabline, so here is how you can define "straight" tabs:

    let g:airline#extensions#tabline#left_sep = ' '
    let g:airline#extensions#tabline#left_alt_sep = '|'

## Seamless integration

vim-airline integrates with a variety of plugins out of the box.  These extensions will be lazily loaded if and only if you have the other plugins installed (and of course you can turn them off).

#### [ctrlp.vim][10]
![image](https://f.cloud.github.com/assets/306502/962258/7345a224-04ec-11e3-8b5a-f11724a47437.png)

#### [unite.vim][9]
![image](https://f.cloud.github.com/assets/306502/962319/4d7d3a7e-04ed-11e3-9d59-ab29cb310ff8.png)

#### [tagbar][19]
![image](https://f.cloud.github.com/assets/306502/962150/7e7bfae6-04ea-11e3-9e28-32af206aed80.png)

#### [csv.vim][28]
![image](https://f.cloud.github.com/assets/306502/962204/cfc1210a-04eb-11e3-8a93-42e6bcd21efa.png)

#### [syntastic][5]
![image](https://f.cloud.github.com/assets/306502/962864/9824c484-04f7-11e3-9928-da94f8c7da5a.png)

#### hunks ([vim-gitgutter][29] & [vim-signify][30])
![image](https://f.cloud.github.com/assets/306502/995185/73fc7054-09b9-11e3-9d45-618406c6ed98.png)

#### [virtualenv][31]
![image](https://f.cloud.github.com/assets/390964/1022566/cf81f830-0d98-11e3-904f-cf4fe3ce201e.png)

## Extras

vim-airline also supplies some supplementary stand-alone extensions.  In addition to the tabline extension mentioned earlier, there is also:

#### whitespace
![image](https://f.cloud.github.com/assets/306502/962401/2a75385e-04ef-11e3-935c-e3b9f0e954cc.png)

## Configurable and extensible

#### Fine-tuned configuration

Every section is composed of parts, and you can reorder and reconfigure them at will.

![image](https://f.cloud.github.com/assets/306502/1073278/f291dd4c-14a3-11e3-8a83-268e2753f97d.png)

Sections can contain accents, which allows for very granular control of visuals (see configuration [here](https://github.com/bling/vim-airline/issues/299#issuecomment-25772886)).

![image](https://f.cloud.github.com/assets/306502/1195815/4bfa38d0-249d-11e3-823e-773cfc2ca894.png)

#### Extensible pipeline

Completely transform the statusline to your liking.  Build out the statusline as you see fit by extracting colors from the current colorscheme's highlight groups.

![allyourbase](https://f.cloud.github.com/assets/306502/1022714/e150034a-0da7-11e3-94a5-ca9d58a297e8.png)

# Rationale

There's already [powerline][2], why yet another statusline?

*  100% vimscript; no python needed.

What about [vim-powerline][1]?

*  vim-powerline has been deprecated in favor of the newer, unifying powerline, which is under active development; the new version is written in python at the core and exposes various bindings such that it can style statuslines not only in vim, but also tmux, bash, zsh, and others.

# Where did the name come from?

I wrote the initial version on an airplane, and since it's light as air it turned out to be a good name.  Thanks for flying vim!

# Installation

This plugin follows the standard runtime path structure, and as such it can be installed with a variety of plugin managers:

*  [Pathogen][11]
  *  `git clone https://github.com/bling/vim-airline ~/.vim/bundle/vim-airline`
*  [NeoBundle][12]
  *  `NeoBundle 'bling/vim-airline'`
*  [Vundle][13]
  *  `Bundle 'bling/vim-airline'`
*  [VAM][22]
  *  `call vam#ActivateAddons([ 'vim-airline' ])`
*  manual
  *  copy all of the files into your `~/.vim` directory

# Configuration

`:help airline`

# Integrating with powerline fonts

For the nice looking powerline symbols to appear, you will need to install a patched font.  Instructions can be found in the official powerline [documentation][20].  Prepatched fonts can be found in the [powerline-fonts][3] repository.

Finally, you can add the convenience variable `let g:airline_powerline_fonts = 1` to your vimrc which will automatically populate the `g:airline_symbols` dictionary with the powerline symbols.

# FAQ

Solutions to common problems can be found in the [Wiki][27].

# Screenshots

A full list of screenshots for various themes can be found in the [Wiki][14].

# Bugs

Tracking down bugs can take a very long time due to different configurations, versions, and operating systems.  To ensure a timely response, please help me out by doing the following:

*  Reproduce it with this [minivimrc][7] repository to rule out any configuration conflicts.
*  A link to your vimrc or a gist which shows how you configured the plugin(s).
*  And so I can reproduce; your `:version` of vim, and the commit of vim-airline you're using.

# Contributions

Contributions and pull requests are welcome.  Please take note of the following guidelines:

*  Adhere to the existing style as much as possible; notably, 2 space indents and long-form keywords.
*  Keep the history clean! squash your branches before you submit a pull request. `pull --rebase` is your friend.
*  Any changes to the core should be tested against Vim 7.2.
*  If you submit a theme, please create a screenshot so it can be added to the [Wiki][14].

# License

MIT License. Copyright (c) 2013 Bailey Ling.


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/bling/vim-airline/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

[1]: https://github.com/Lokaltog/vim-powerline
[2]: https://github.com/Lokaltog/powerline
[3]: https://github.com/Lokaltog/powerline-fonts
[4]: https://github.com/tpope/vim-fugitive
[5]: https://github.com/scrooloose/syntastic
[6]: https://github.com/bling/vim-bufferline
[7]: https://github.com/bling/minivimrc
[8]: http://en.wikipedia.org/wiki/Open/closed_principle
[9]: https://github.com/Shougo/unite.vim
[10]: https://github.com/kien/ctrlp.vim
[11]: https://github.com/tpope/vim-pathogen
[12]: https://github.com/Shougo/neobundle.vim
[13]: https://github.com/gmarik/vundle
[14]: https://github.com/bling/vim-airline/wiki/Screenshots
[15]: https://github.com/techlivezheng/vim-plugin-minibufexpl
[16]: https://github.com/sjl/gundo.vim
[17]: https://github.com/mbbill/undotree
[18]: https://github.com/scrooloose/nerdtree
[19]: https://github.com/majutsushi/tagbar
[20]: https://powerline.readthedocs.org/en/latest/fontpatching.html
[21]: https://bitbucket.org/ludovicchabant/vim-lawrencium
[22]: https://github.com/MarcWeber/vim-addon-manager
[23]: https://github.com/altercation/solarized
[24]: https://github.com/chriskempson/tomorrow-theme
[25]: https://github.com/tomasr/molokai
[26]: https://github.com/nanotech/jellybeans.vim
[27]: https://github.com/bling/vim-airline/wiki/FAQ
[28]: https://github.com/chrisbra/csv.vim
[29]: https://github.com/airblade/vim-gitgutter
[30]: https://github.com/mhinz/vim-signify
[31]: https://github.com/jmcantrell/vim-virtualenv
[32]: https://github.com/chriskempson/base16-vim
[33]: https://github.com/bling/vim-airline/wiki/Test-Plan
[34]: http://eclim.org
