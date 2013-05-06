This project adds [CoffeeScript] support to the vim editor. It handles syntax,
indenting, compiling, and more. Also included is support for CoffeeScript in
Haml and HTML.

![Screenshot](http://i.imgur.com/eUBvm.png)

[CoffeeScript]: http://jashkenas.github.com/coffee-script/

### Install from a Zipball

This is the quickest way to get things running.

1. Download the latest zipball from [vim.org][zipball-vim] or
   [github][zipball-github]. The latest version on github is under Download
   Packages (don't use the Download buttons.)

2. Extract the archive into `~/.vim/`:

        unzip -od ~/.vim vim-coffee-script-HASH.zip

These steps are also used to update the plugin.

[zipball-vim]: http://www.vim.org/scripts/script.php?script_id=3590
[zipball-github]: https://github.com/kchmck/vim-coffee-script/downloads

### Install with Pathogen

Since this plugin has rolling versions based on git commits, using pathogen and
git is the preferred way to install. The plugin ends up contained in its own
directory and updates are just a `git pull` away.

1. Install tpope's [pathogen] into `~/.vim/autoload/` and add this line to your
   `vimrc`:

        call pathogen#infect()

    To get the all the features of this plugin, make sure you also have a
    `filetype plugin indent on` line in there.

[pathogen]: http://www.vim.org/scripts/script.php?script_id=2332

2. Create and change into `~/.vim/bundle/`:

        $ mkdir ~/.vim/bundle
        $ cd ~/.vim/bundle

3. Make a clone of the `vim-coffee-script` repository:

        $ git clone https://github.com/kchmck/vim-coffee-script.git

#### Updating

1. Change into `~/.vim/bundle/vim-coffee-script/`:

        $ cd ~/.vim/bundle/vim-coffee-script

2. Pull in the latest changes:

        $ git pull

### CoffeeMake: Compile the Current File

The `CoffeeMake` command compiles the current file and parses any errors:

  ![CoffeeMake](http://i.imgur.com/cr9xI.png)

The full signature of the command is:

    :[silent] CoffeeMake[!] [COFFEE-OPTIONS]...

By default, `CoffeeMake` shows all compiler output and jumps to the first line
reported as an error by `coffee`:

    :CoffeeMake

Compiler output can be hidden with `silent`:

    :silent CoffeeMake

Line-jumping can be turned off by adding a bang:

    :CoffeeMake!

Options given to `CoffeeMake` are passed along to `coffee`:

    :CoffeeMake --bare

`CoffeeMake` can be manually loaded for a file with:

    :compiler coffee

#### Recompile on write

To recompile a file when it's written, add an `autocmd` like this to your
`vimrc`:

    au BufWritePost *.coffee silent CoffeeMake!

All of the customizations above can be used, too. This one compiles silently
and with the `-b` option, but shows any errors:

    au BufWritePost *.coffee silent CoffeeMake! -b | cwindow | redraw!

The `redraw!` command is needed to fix a redrawing quirk in terminal vim, but
can removed for gVim.

#### Default compiler options

The `CoffeeMake` command passes any options in the `coffee_make_options`
variable along to the compiler. You can use this to set default options:

    let coffee_make_options = '--bare'

#### Path to compiler

To change the compiler used by `CoffeeMake` and `CoffeeCompile`, set
`coffee_compiler` to the full path of an executable or the filename of one
in your `$PATH`:

    let coffee_compiler = '/usr/bin/coffee'

This option is set to `coffee` by default.

### CoffeeCompile: Compile Snippets of CoffeeScript

The `CoffeeCompile` command shows how the current file or a snippet of
CoffeeScript is compiled to JavaScript. The full signature of the command is:

    :[RANGE] CoffeeCompile [watch|unwatch] [vert[ical]] [WINDOW-SIZE]

Calling `CoffeeCompile` without a range compiles the whole file:

  ![CoffeeCompile](http://i.imgur.com/KJfSZ.png)

Calling `CoffeeCompile` with a range, like in visual mode, compiles the selected
snippet of CoffeeScript:

  ![CoffeeCompile Snippet](http://i.imgur.com/mbaUA.png)

  ![Compiled Snippet](http://i.imgur.com/Ocjuc.png)

This scratch buffer can be quickly closed by hitting the `q` key.

Using `vert` splits the CoffeeCompile buffer vertically instead of horizontally:

    :CoffeeCompile vert

Set the `coffee_compile_vert` variable to split the buffer vertically by
default:

    let coffee_compile_vert = 1

The initial size of the CoffeeCompile buffer can be given as a number:

    :CoffeeCompile 4

#### Watch (live preview) mode

Watch mode is like the Try CoffeeScript preview box on the CoffeeScript
homepage:

  ![Watch Mode](http://i.imgur.com/M6l1j.png)

  ![Watch Mode](http://i.imgur.com/qtNmU.png)

Writing some code and then exiting insert mode automatically updates the
compiled JavaScript buffer.

Use `watch` to start watching a buffer (`vert` is also recommended):

    :CoffeeCompile watch vert

After making some changes in insert mode, hit escape and the CoffeeScript will
be recompiled. Changes made outside of insert mode don't trigger this recompile,
but calling `CoffeeCompile` will compile these changes without any bad effects.

To get synchronized scrolling of a CoffeeScript and CoffeeCompile buffer, set
`scrollbind` on each:

    :setl scrollbind

Use `unwatch` to stop watching a buffer:

    :CoffeeCompile unwatch

### CoffeeLint: Lint your CoffeeScript

The `CoffeeLint` command runs [coffeelint](http://www.coffeelint.org/) (version
0.4.0 or later required) on the current file and parses any errors:

    :[RANGE] CoffeeLint[!] [COFFEELINT-OPTIONS]

Use it like `CoffeeMake`.

  ![CoffeeLint](http://i.imgur.com/dlxF7.png)

#### Default coffeelint options

Options in `coffee_lint_options` are passed along to `coffeelint`:

    let coffee_lint_options = '-f lint.json'

#### Path to `coffeelint`

Use the `coffee_linter` option to set a different path to the `coffeelint`
executable:

    let coffee_linter = '/usr/bin/coffeelint'

This option is set to `coffeelint` by default.

### CoffeeRun: Run some CoffeeScript

The `CoffeeRun` command compiles the current file or selected snippet and runs
the resulting JavaScript. Output is shown at the bottom of the screen:

  ![CoffeeRun](http://i.imgur.com/7hwSy.png)

  ![CoffeeRun Output](http://i.imgur.com/WNWvC.png)

### Configure Syntax Highlighting

Add these lines to your `vimrc` to disable the relevant syntax group.

#### Disable trailing whitespace error

Trailing whitespace is highlighted as an error by default. This can be disabled
with:

    hi link coffeeSpaceError NONE

#### Disable trailing semicolon error

Trailing semicolons are also considered an error (for help transitioning from
JavaScript.) This can be disabled with:

    hi link coffeeSemicolonError NONE

#### Disable reserved words error

Reserved words like `function` and `var` are highlighted as an error where
they're not allowed in CoffeeScript. This can be disabled with:

    hi link coffeeReservedError NONE

### Tune Vim for CoffeeScript

Changing these core settings can make vim more CoffeeScript friendly.

#### Fold by indentation

Folding by indentation works well for CoffeeScript functions and classes:

  ![Folding](http://i.imgur.com/Y0rDC.png)

To fold by indentation in CoffeeScript files, add this line to your `vimrc`:

    au BufNewFile,BufReadPost *.coffee setl foldmethod=indent nofoldenable

With this, folding is disabled by default but can be quickly toggled per-file
by hitting `zi`. To enable folding by default, remove `nofoldenable`:

    au BufNewFile,BufReadPost *.coffee setl foldmethod=indent

#### Two-space indentation

To get standard two-space indentation in CoffeeScript files, add this line to
your `vimrc`:

    au BufNewFile,BufReadPost *.coffee setl shiftwidth=2 expandtab
