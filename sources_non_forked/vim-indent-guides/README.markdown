# Indent Guides
Indent Guides is a plugin for visually displaying indent levels in Vim.

<img src="http://i.imgur.com/ONgoj.png" width="448" height="448" alt="" />

## Features:
* Can detect both tab and space indent styles.
* Automatically inspects your colorscheme and picks appropriate colors (gVim only).
* Will highlight indent levels with alternating colors.
* Full support for gVim and basic support for Terminal Vim.
* Seems to work on Windows gVim 7.3 (haven't done any extensive tests though).
* Customizable size for indent guides, eg. skinny guides (soft-tabs only).
* Customizable start indent level.
* **NEW:** Highlight support for files with a mixture of tab and space indent styles.

## Requirements
* Vim 7.2+

## Installation
To install the plugin just copy `autoload`, `plugin`, `doc` directories into your `.vim` directory.

Alternatively if you have [Pathogen](http://www.vim.org/scripts/script.php?script_id=2332) installed, just clone this repo into a subdirectory of your `.vim/bundle` directory like so:

    cd ~/.vim/bundle
    git clone git://github.com/nathanaelkane/vim-indent-guides.git

## Usage
The default mapping to toggle the plugin is `<Leader>ig`

### gVim
**This plugin should work with gVim out of the box, no configuration needed.** It will automatically inspect your colorscheme and pick appropriate colors.

### Setting custom indent colors
Here's an example of how to define custom colors instead of using the ones the plugin automatically generates for you. Add this to your `.vimrc` file:

    let g:indent_guides_auto_colors = 0
    autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=red   ctermbg=3
    autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=green ctermbg=4

Alternatively you can add the following lines to your colorscheme file.

    hi IndentGuidesOdd  guibg=red   ctermbg=3
    hi IndentGuidesEven guibg=green ctermbg=4

### Terminal Vim
At the moment Terminal Vim only has basic support. This means is that colors won't be automatically calculated based on your colorscheme. Instead, some preset colors are used depending on whether `background` is set to `dark` or `light`.

When `set background=dark` is used, the following highlight colors will be defined:

    hi IndentGuidesOdd  ctermbg=black
    hi IndentGuidesEven ctermbg=darkgrey

Alternatively, when `set background=light` is used, the following highlight colors will be defined:

    hi IndentGuidesOdd  ctermbg=white
    hi IndentGuidesEven ctermbg=lightgrey

If for some reason it's incorrectly defining light highlight colors instead of dark ones or vice versa, the first thing you should check is that the `background` value is being set correctly for your colorscheme. Sometimes it's best to manually set the `background` value in your `.vimrc`, for example:

    colorscheme desert256
    set background=dark

Alternatively you can manually setup the highlight colors yourself, see `:help indent_guides_auto_colors` for an example.

## Help
`:help indent-guides`

## Screenshots
<img src="http://i.imgur.com/7tMBl.png" width="448" height="448" alt="" />
<img src="http://i.imgur.com/EvrqK.png" width="448" height="448" alt="" />
<img src="http://i.imgur.com/hHqp2.png" width="448" height="448" alt="" />
