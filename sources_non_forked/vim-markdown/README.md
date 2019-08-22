# Vim Markdown

[![Build Status](https://travis-ci.org/plasticboy/vim-markdown.svg)](https://travis-ci.org/plasticboy/vim-markdown)

Syntax highlighting, matching rules and mappings for [the original Markdown](http://daringfireball.net/projects/markdown/) and extensions.

1. [Installation](#installation)
1. [Basic usage](#basic-usage)
1. [Options](#options)
1. [Mappings](#mappings)
1. [Commands](#commands)
1. [Credits](#credits)
1. [License](#license)

## Installation

If you use [Vundle](https://github.com/gmarik/vundle), add the following lines to your `~/.vimrc`:

```vim
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
```

The `tabular` plugin must come *before* `vim-markdown`.

Then run inside Vim:

```vim
:so ~/.vimrc
:PluginInstall
```

If you use [Pathogen](https://github.com/tpope/vim-pathogen), do this:

```sh
cd ~/.vim/bundle
git clone https://github.com/plasticboy/vim-markdown.git
```

To install without Pathogen using the Debian [vim-addon-manager](http://packages.qa.debian.org/v/vim-addon-manager.html), do this:

```sh
git clone https://github.com/plasticboy/vim-markdown.git
cd vim-markdown
sudo make install
vim-addon-manager install markdown
```

If you are not using any package manager, download the [tarball](https://github.com/plasticboy/vim-markdown/archive/master.tar.gz) and do this:

```sh
cd ~/.vim
tar --strip=1 -zxf vim-markdown-master.tar.gz
```

## Basic usage

### Folding

Folding is enabled for headers by default.

The following commands are useful to open and close folds:

- `zr`: reduces fold level throughout the buffer
- `zR`: opens all folds
- `zm`: increases fold level throughout the buffer
- `zM`: folds everything all the way
- `za`: open a fold your cursor is on
- `zA`: open a fold your cursor is on recursively
- `zc`: close a fold your cursor is on
- `zC`: close a fold your cursor is on recursively

[Options](#options) are available to disable folding or change folding style.

Try `:help fold-expr` and `:help fold-commands` for details.

### Concealing

Concealing is set for some syntax such as bold, italic, code block and link.

Concealing lets you conceal text with other text. The actual source text is not modified. If you put your cursor on the concealed line, the conceal goes away.

[Options](#options) are available to disable or change concealing.

Try `:help concealcursor` and `:help conceallevel` for details.

## Options

### Disable Folding

-   `g:vim_markdown_folding_disabled`

    Add the following line to your `.vimrc` to disable the folding configuration:

        let g:vim_markdown_folding_disabled = 1

    This option only controls Vim Markdown specific folding configuration.

    To enable/disable folding use Vim's standard folding configuration.

        set [no]foldenable

### Change fold style

-   `g:vim_markdown_folding_style_pythonic`

    To fold in a style like [python-mode](https://github.com/klen/python-mode), add the following to your `.vimrc`:

        let g:vim_markdown_folding_style_pythonic = 1

    `g:vim_markdown_folding_level` setting (default 1) is set to `foldlevel`.
    Thus level 1 heading which is served as a document title is expanded by default.

-   `g:vim_markdown_override_foldtext`

    To prevent foldtext from being set add the following to your `.vimrc`:

        let g:vim_markdown_override_foldtext = 0

### Set header folding level

-   `g:vim_markdown_folding_level`

    Folding level is a number between 1 and 6. By default, if not specified, it is set to 1.

        let g:vim_markdown_folding_level = 6

    Tip: it can be changed on the fly with:

        :let g:vim_markdown_folding_level = 1
        :edit

### Disable Default Key Mappings

-   `g:vim_markdown_no_default_key_mappings`

    Add the following line to your `.vimrc` to disable default key mappings:

        let g:vim_markdown_no_default_key_mappings = 1

    You can also map them by yourself with `<Plug>` mappings.

### Enable TOC window auto-fit

-   `g:vim_markdown_toc_autofit`

    Allow for the TOC window to auto-fit when it's possible for it to shrink.
    It never increases its default size (half screen), it only shrinks.

        let g:vim_markdown_toc_autofit = 1

### Text emphasis restriction to single-lines

-   `g:vim_markdown_emphasis_multiline`

    By default text emphasis works across multiple lines until a closing token is found. However, it's possible to restrict text emphasis to a single line (i.e., for it to be applied a closing token must be found on the same line). To do so:

        let g:vim_markdown_emphasis_multiline = 0

### Syntax Concealing

-   `g:vim_markdown_conceal`

    Concealing is set for some syntax.

    For example, conceal `[link text](link url)` as just `link text`.
    Also, `_italic_` and `*italic*` will conceal to just _italic_.
    Similarly `__bold__`, `**bold**`, `___italic bold___`, and `***italic bold***`
    will conceal to just __bold__, **bold**, ___italic bold___, and ***italic bold*** respectively.

    To enable conceal use Vim's standard conceal configuration.

        set conceallevel=2

    To disable conceal regardless of `conceallevel` setting, add the following to your `.vimrc`:

        let g:vim_markdown_conceal = 0

    To disable math conceal with LaTeX math syntax enabled, add the following to your `.vimrc`:

        let g:tex_conceal = ""
        let g:vim_markdown_math = 1

-   `g:vim_markdown_conceal_code_blocks`

    Disabling conceal for code fences requires an additional setting:

        let g:vim_markdown_conceal_code_blocks = 0

### Fenced code block languages

-   `g:vim_markdown_fenced_languages`

    You can use filetype name as fenced code block languages for syntax highlighting.
    If you want to use different name from filetype, you can add it in your `.vimrc` like so:

        let g:vim_markdown_fenced_languages = ['csharp=cs']

    This will cause the following to be highlighted using the `cs` filetype syntax.

        ```csharp
        ...
        ```

    Default is `['c++=cpp', 'viml=vim', 'bash=sh', 'ini=dosini']`.

### Follow named anchors

-   `g:vim_markdown_follow_anchor`

    This feature allows the `ge` command to follow named anchors in links of the form
    `file#anchor` or just `#anchor`, where file may omit the `.md` extension as
    usual. Two variables control its operation:

        let g:vim_markdown_follow_anchor = 1

    This tells vim-markdown whether to attempt to follow a named anchor in a link or
    not. When it is 1, and only if a link can be split in two parts by the pattern
    '#', then the first part is interpreted as the file and the second one as the
    named anchor. This also includes urls of the form `#anchor`, for which the first
    part is considered empty, meaning that the target file is the current one. After
    the file is opened, the anchor will be searched.

    Default is `0`.

-   `g:vim_markdown_anchorexpr`

        let g:vim_markdown_anchorexpr = "'<<'.v:anchor.'>>'"

    This expression will be evaluated substituting `v:anchor` with a quoted string
    that contains the anchor to visit. The result of the evaluation will become the
    real anchor to search in the target file. This is useful in order to convert
    anchors of the form, say, `my-section-title` to searches of the form `My Section
    Title` or `<<my-section-title>>`.

    Default is `''`.

### Syntax extensions

The following options control which syntax extensions will be turned on. They are off by default.

#### LaTeX math

-   `g:vim_markdown_math`

    Used as `$x^2$`, `$$x^2$$`, escapable as `\$x\$` and `\$\$x\$\$`.

        let g:vim_markdown_math = 1

#### YAML Front Matter

-   `g:vim_markdown_frontmatter`

    Highlight YAML front matter as used by Jekyll or [Hugo](https://gohugo.io/content/front-matter/).

        let g:vim_markdown_frontmatter = 1

#### TOML Front Matter

-   `g:vim_markdown_toml_frontmatter`

    Highlight TOML front matter as used by [Hugo](https://gohugo.io/content/front-matter/).

    TOML syntax highlight requires [vim-toml](https://github.com/cespare/vim-toml).

        let g:vim_markdown_toml_frontmatter = 1

#### JSON Front Matter

-   `g:vim_markdown_json_frontmatter`

    Highlight JSON front matter as used by [Hugo](https://gohugo.io/content/front-matter/).

    JSON syntax highlight requires [vim-json](https://github.com/elzr/vim-json).

        let g:vim_markdown_json_frontmatter = 1

#### Strikethrough

-   `g:vim_markdown_strikethrough`

    Strikethrough uses two tildes. `~~Scratch this.~~`

        let g:vim_markdown_strikethrough = 1

### Adjust new list item indent

-   `g:vim_markdown_new_list_item_indent`

    You can adjust a new list indent. For example, you insert a single line like below:

        * item1

    Then if you type `o` to insert new line in vim and type `* item2`, the result will be:

        * item1
            * item2

    vim-markdown automatically insert the indent. By default, the number of spaces of indent is 4. If you'd like to change the number as 2, just write:

        let g:vim_markdown_new_list_item_indent = 2

### Do not require .md extensions for Markdown links

-   `g:vim_markdown_no_extensions_in_markdown`

    If you want to have a link like this `[link text](link-url)` and follow it for editing in vim using the `ge` command, but have it open the file "link-url.md" instead of the file "link-url", then use this option:

        let g:vim_markdown_no_extensions_in_markdown = 1

    This is super useful for GitLab and GitHub wiki repositories.

    Normal behaviour would be that vim-markup required you to do this `[link text](link-url.md)`, but this is not how the Gitlab and GitHub wiki repositories work. So this option adds some consistency between the two.

### Auto-write when following link

-   `g:vim_markdown_autowrite`

    If you follow a link like this `[link text](link-url)` using the `ge` shortcut, this option will automatically save any edits you made before moving you:

        let g:vim_markdown_autowrite = 1

### Change default file extension

-   `g:vim_markdown_auto_extension_ext`

    If you would like to use a file extension other than `.md` you may do so using the `vim_markdown_auto_extension_ext` variable:

        let g:vim_markdown_auto_extension_ext = 'txt'

### Do not automatically insert bulletpoints

-   `g:vim_markdown_auto_insert_bullets`

    Automatically inserting bulletpoints can lead to problems when wrapping text
    (see issue #232 for details), so it can be disabled:

        let g:vim_markdown_auto_insert_bullets = 0

    In that case, you probably also want to set the new list item indent to 0 as
    well, or you will have to remove an indent each time you add a new list item:

        let g:vim_markdown_new_list_item_indent = 0

### Change how to open new files

-   `g:vim_markdown_edit_url_in`

    By default when following a link the target file will be opened in your current buffer.  This behavior can change if you prefer using splits or tabs by using the `vim_markdown_edit_url_in` variable.  Possible values are `tab`, `vsplit`, `hsplit`, `current` opening in a new tab, vertical split, horizontal split, and current buffer respectively.  Defaults to current buffer if not set:

        let g:vim_markdown_edit_url_in = 'tab'

## Mappings

The following work on normal and visual modes:

-   `gx`: open the link under the cursor in the same browser as the standard `gx` command. `<Plug>Markdown_OpenUrlUnderCursor`

    The standard `gx` is extended by allowing you to put your cursor anywhere inside a link.

    For example, all the following cursor positions will work:

        [Example](http://example.com)
        ^  ^    ^^   ^       ^
        1  2    34   5       6

        <http://example.com>
        ^  ^               ^
        1  2               3

    Known limitation: does not work for links that span multiple lines.

-   `ge`: open the link under the cursor in Vim for editing. Useful for relative markdown links. `<Plug>Markdown_EditUrlUnderCursor`

    The rules for the cursor position are the same as the `gx` command.

-   `]]`: go to next header. `<Plug>Markdown_MoveToNextHeader`

-   `[[`: go to previous header. Contrast with `]c`. `<Plug>Markdown_MoveToPreviousHeader`

-   `][`: go to next sibling header if any. `<Plug>Markdown_MoveToNextSiblingHeader`

-   `[]`: go to previous sibling header if any. `<Plug>Markdown_MoveToPreviousSiblingHeader`

-   `]c`: go to Current header. `<Plug>Markdown_MoveToCurHeader`

-   `]u`: go to parent header (Up). `<Plug>Markdown_MoveToParentHeader`

This plugin follows the recommended Vim plugin mapping interface, so to change the map `]u` to `asdf`, add to your `.vimrc`:

    map asdf <Plug>Markdown_MoveToParentHeader

To disable a map use:

    map <Plug> <Plug>Markdown_MoveToParentHeader

## Commands

The following requires `:filetype plugin on`.

-   `:HeaderDecrease`:

    Decrease level of all headers in buffer: `h2` to `h1`, `h3` to `h2`, etc.

    If range is given, only operate in the range.

    If an `h1` would be decreased, abort.

    For simplicity of implementation, Setex headers are converted to Atx.

-   `:HeaderIncrease`: Analogous to `:HeaderDecrease`, but increase levels instead.

-   `:SetexToAtx`:

    Convert all Setex style headers in buffer to Atx.

    If a range is given, e.g. hit `:` from visual mode, only operate on the range.

-   `:TableFormat`: Format the table under the cursor [like this](http://www.cirosantilli.com/markdown-style-guide/#tables).

    Requires [Tabular](https://github.com/godlygeek/tabular).

    The input table *must* already have a separator line as the second line of the table.
    That line only needs to contain the correct pipes `|`, nothing else is required.

-   `:Toc`: create a quickfix vertical window navigable table of contents with the headers.

    Hit `<Enter>` on a line to jump to the corresponding line of the markdown file.

-   `:Toch`: Same as `:Toc` but in an horizontal window.

-   `:Toct`: Same as `:Toc` but in a new tab.

-   `:Tocv`: Same as `:Toc` for symmetry with `:Toch` and `:Tocv`.

## Credits

The main contributors of vim-markdown are:

- **Ben Williams** (A.K.A. **plasticboy**). The original developer of vim-markdown. [Homepage](http://plasticboy.com/).

If you feel that your name should be on this list, please make a pull request listing your contributions.

## License

The MIT License (MIT)

Copyright (c) 2012 Benjamin D. Williams

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
