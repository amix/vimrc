This project adds [CoffeeScript] support to vim. It covers syntax, indenting,
compiling, and more.

![Screenshot](http://i.imgur.com/j1BhpZQ.png)

[CoffeeScript]: http://coffeescript.org/

## Table of Contents

- Installation
  - [Requirements](#requirements)
  - [Install using Pathogen](#install-using-pathogen)
  - [Install using Vundle](#install-using-vundle)
  - [Install from a Zip File](#install-from-a-zip-file)
- Coffee Commands
  - [Compile to JavaScript](#compile-to-javascript)
  - [Compile CoffeeScript Snippets](#coffeecompile-compile-coffeescript-snippets)
  - [Live Preview Compiling](#coffeewatch-live-preview-compiling)
  - [Run CoffeeScript Snippets](#coffeerun-run-coffeescript-snippets)
  - [Lint your CoffeeScript](#coffeelint-lint-your-coffeescript)
- Extras
  - [Literate CoffeeScript](#literate-coffeescript)
  - [CoffeeScript in HTML](#coffeescript-in-html)
  - [CoffeeScript in Haml](#coffeescript-in-haml)
- Configuration
  - [Custom Autocmds](#custom-autocmds)
  - [Configuration Variables](#configuration-variables)
  - [Configure Syntax Highlighting](#configure-syntax-highlighting)
  - [Tune Vim for CoffeeScript](#tune-vim-for-coffeescript)

## Requirements

 - vim 7.4 or later
 - coffee 1.2.0 or later

## Install using Pathogen

This project uses rolling releases based on git commits, so pathogen is a
natural fit for it. If you're already using pathogen, you can skip to step 4.

1. Install [pathogen.vim] into `~/.vim/autoload/` (see [pathogen's
   readme][install-pathogen] for more information.)

[pathogen.vim]: http://www.vim.org/scripts/script.php?script_id=2332
[install-pathogen]: https://github.com/tpope/vim-pathogen#installation

2. Enable pathogen in your vimrc. Here's a bare-minimum vimrc that enables
   all the features of `vim-coffee-script`:

   ```vim
   call pathogen#infect()
   syntax enable
   filetype plugin indent on
   ```

   If you already have a vimrc built up, just make sure it contains these calls,
   in this order.

3. Create the directory `~/.vim/bundle/`:

        mkdir ~/.vim/bundle

4. Clone the `vim-coffee-script` repo into `~/.vim/bundle/`:

        git clone https://github.com/kchmck/vim-coffee-script.git ~/.vim/bundle/vim-coffee-script/

Updating takes two steps:

1. Change into `~/.vim/bundle/vim-coffee-script/`:

        cd ~/.vim/bundle/vim-coffee-script

2. Pull in the latest changes:

        git pull

## Install using Vundle

1. [Install Vundle] into `~/.vim/bundle/`.

[Install Vundle]: https://github.com/gmarik/vundle#quick-start

2. Configure your vimrc for Vundle. Here's a bare-minimum vimrc that enables all
   the features of `vim-coffee-script`:


   ```vim
   set nocompatible
   filetype off

   set rtp+=~/.vim/bundle/vundle/
   call vundle#rc()

   Plugin 'kchmck/vim-coffee-script'

   syntax enable
   filetype plugin indent on
   ```

   If you're adding Vundle to a built-up vimrc, just make sure all these calls
   are in there and that they occur in this order.

3. Open vim and run `:PluginInstall`.

To update, open vim and run `:PluginInstall!` (notice the bang!)

## Install from a Zip File

1. Download the latest zip file from [vim.org][zip].

2. Extract the archive into `~/.vim/`:

        unzip -od ~/.vim/ ARCHIVE.zip

   This should create the files `~/.vim/autoload/coffee.vim`,
   `~/.vim/compiler/coffee.vim`, etc.

You can update the plugin using the same steps.

[zip]: http://www.vim.org/scripts/script.php?script_id=3590

## Compile to JavaScript

A `coffee` wrapper for use with `:make` is enabled automatically for coffee
files if no other compiler is loaded. To enable it manually, run

    :compiler coffee

The `:make` command is then configured to use the `coffee` compiler and
recognize its errors. I've included a quick reference here but be sure to check
out [`:help :make`][make] for a full reference of the command.

  ![make](http://i.imgur.com/scUXmxR.png)

  ![make Result](http://i.imgur.com/eGIjEdn.png)

[make]: http://vimdoc.sourceforge.net/htmldoc/quickfix.html#:make_makeprg

Consider the full signature of a `:make` call as

    :[silent] make[!] [COFFEE-OPTIONS]...

By default `:make` shows all compiler output and jumps to the first line
reported as an error. Compiler output can be hidden with a leading `:silent`:

    :silent make

Line-jumping can be turned off by adding a bang:

    :make!

`COFFEE-OPTIONS` given to `:make` are passed along to `coffee` (see also
[`coffee_make_options`](#coffee_make_options)):

    :make --bare --output /some/dir

See the [full table of options](http://coffeescript.org/#usage) for a
list of all the options that `coffee` recognizes.

*Configuration*: [`coffee_compiler`](#coffee_compiler),
[`coffee_make_options`](#coffee_make_options)

#### The quickfix window

Compiler errors are added to the [quickfix] list by `:make`, but the quickfix
window isn't automatically shown. The [`:cwindow`][cwindow] command will pop up
the quickfix window if there are any errors:

    :make
    :cwindow

This is usually the desired behavior, so you may want to add an autocmd to your
vimrc to do this automatically:

    autocmd QuickFixCmdPost * nested cwindow | redraw!

The `redraw!` command is needed to fix a redrawing quirk in terminal vim, but
can removed for gVim.

[quickfix]: http://vimdoc.sourceforge.net/htmldoc/quickfix.html#quickfix
[cwindow]: http://vimdoc.sourceforge.net/htmldoc/quickfix.html#:cwindow

#### Recompile on write

To recompile a file when it's written, add a `BufWritePost` autocmd to your
vimrc:

    autocmd BufWritePost *.coffee silent make!

#### Cake and Cakefiles

A `cake` compiler is also available with the call

    :compiler cake

You can then use `:make` as above to run your Cakefile and capture any `coffee`
errors:

    :silent make build

It runs within the current directory, so make sure you're in the directory of
your Cakefile before calling it.

*Configuration*: [`coffee_cake`](#coffee_cake),
[`coffee_cake_options`](#coffee_cake_options)

## CoffeeCompile: Compile CoffeeScript Snippets

CoffeeCompile shows how the current file or a snippet of CoffeeScript is
compiled to JavaScript.

    :[RANGE] CoffeeCompile [vert[ical]] [WINDOW-SIZE]

Calling `:CoffeeCompile` without a range compiles the whole file:

  ![CoffeeCompile](http://i.imgur.com/0zFG0l0.png)

  ![CoffeeCompile Result](http://i.imgur.com/bpiAxaa.png)

Calling it with a range, like in visual mode, compiles only the selected snippet
of CoffeeScript:

  ![CoffeeCompile Snippet](http://i.imgur.com/x3OT3Ay.png)

  ![Compiled Snippet](http://i.imgur.com/J02j4T8.png)

Each file gets its own CoffeeCompile buffer, and the same buffer is used for all
future calls of `:CoffeeCompile` on that file. It can be quickly closed by
hitting `q` in normal mode.

Using `vert` opens the CoffeeCompile buffer vertically instead of horizontally
(see also [`coffee_compile_vert`](#coffee_compile_vert)):

    :CoffeeCompile vert

By default the CoffeeCompile buffer splits the source buffer in half, but this
can be overridden by passing in a `WINDOW-SIZE`:

    :CoffeeCompile 4

*Configuration*: [`coffee_compiler`](#coffee_compiler`),
[`coffee_compile_vert`](#coffee_compile_vert)

#### Quick syntax checking

If compiling a snippet results in a compiler error, CoffeeCompile adds that
error to the [quickfix] list.

[quickfix]: http://vimdoc.sourceforge.net/htmldoc/quickfix.html#quickfix

  ![Syntax Checking](http://i.imgur.com/RC8accF.png)

  ![Syntax Checking Result](http://i.imgur.com/gi1ON75.png)

You can use this to quickly check the syntax of a snippet.

## CoffeeWatch: Live Preview Compiling

CoffeeWatch emulates using the Try CoffeeScript preview box on the [CoffeeScript
homepage][CoffeeScript].

  ![CoffeeWatch](http://i.imgur.com/TRHdIMG.png)

  ![CoffeeWatch Result](http://i.imgur.com/rJbOeeS.png)

CoffeeWatch takes the same options as CoffeeCompile:

    :CoffeeWatch [vert[ical]] [WINDOW-SIZE]

After a source buffer is watched, leaving insert mode or saving the file fires
off a recompile of the CoffeeScript:

  ![Insert Mode](http://i.imgur.com/SBVcf4k.png)

  ![Recompile](http://i.imgur.com/pbPMog7.png)

You can force recompilation by calling `:CoffeeWatch`.

To get synchronized scrolling of the source buffer and CoffeeWatch buffer, set
[`'scrollbind'`](http://vimdoc.sourceforge.net/htmldoc/options.html#'scrollbind')
on each:

    :setl scrollbind

*Configuration*: [`coffee_compiler`](#coffee_compiler),
[`coffee_watch_vert`](#coffee_watch_vert)

## CoffeeRun: Run CoffeeScript Snippets

CoffeeRun compiles the current file or selected snippet and runs the resulting
JavaScript.

  ![CoffeeRun](http://i.imgur.com/YSkHUuQ.png)

  ![CoffeeRun Output](http://i.imgur.com/wZQbggN.png)

The command has two forms:

    :CoffeeRun [PROGRAM-OPTIONS]...

This form applies when no `RANGE` is given or when the given range is `1,$`
(first line to last line). It allows passing `PROGRAM-OPTIONS` to your compiled
program. The filename is passed directly to `coffee` so you must save the file
for your changes to take effect.

    :RANGE CoffeeRun [COFFEE-OPTIONS]...

This form applies with all other ranges. It compiles and runs the lines within
the given `RANGE` and any extra `COFFEE-OPTIONS` are passed to `coffee`.

*Configuration*: [`coffee_compiler`](#coffee_compiler),
[`coffee_run_vert`](#coffee_run_vert)

## CoffeeLint: Lint your CoffeeScript

CoffeeLint runs [coffeelint](http://www.coffeelint.org/) (version 1.4.0 or later
required) on the current file and adds any issues to the [quickfix] list.

  ![CoffeeLint](http://i.imgur.com/UN8Nr5N.png)

  ![CoffeeLint Result](http://i.imgur.com/9hSIj3W.png)

    :[RANGE] CoffeeLint[!] [COFFEELINT-OPTIONS]... [ | cwindow]

If a `RANGE` is given, only those lines are piped to `coffeelint`. Options given
in `COFFEELINT-OPTIONS` are passed to `coffeelint` (see also
[`coffee_lint_options`](#coffee_lint_options)):

    :CoffeeLint -f lint.json

It behaves very similar to `:make`, described [above](#compile-to-javascript).

    :CoffeeLint! | cwindow

*Configuration*: [`coffee_linter`](#coffee_linter),
[`coffee_lint_options`](#coffee_lint_options)

## Literate CoffeeScript

Literate CoffeeScript syntax and indent support is provided by
[vim-literate-coffeescript]. The `Coffee` commands detect when they're running
on a litcoffee file and pass the `--literate` flag to their respective tools,
but at this time the commands are not automatically loaded when a litcoffee file
is opened.

[vim-literate-coffeescript]: https://github.com/mintplant/vim-literate-coffeescript

To load them, run

    runtime ftplugin/coffee.vim

while inside a litcoffee buffer. To do this automatically, add

    autocmd FileType litcoffee runtime ftplugin/coffee.vim

to your vimrc.

## CoffeeScript in HTML

CoffeeScript is highlighted and indented within

```html
<script type="text/coffeescript">
</script>
```

blocks in html files.

## CoffeeScript in Haml

CoffeeScript is highlighted within the `:coffeescript` filter in haml files:

```haml
:coffeescript
  console.log "hullo"
```

At this time, coffee indenting doesn't work in these blocks.

## Custom Autocmds

You can [define commands][autocmd-explain] to be ran automatically on these
custom events.

In all cases, the name of the command running the event (`CoffeeCompile`,
`CoffeeWatch`, or `CoffeeRun`) is matched by the [`{pat}`][autocmd] argument.
You can match all commands with a `*` or only specific commands by separating
them with a comma: `CoffeeCompile,CoffeeWatch`.

[autocmd-explain]: http://vimdoc.sourceforge.net/htmldoc/usr_40.html#40.3
[autocmd]: http://vimdoc.sourceforge.net/htmldoc/autocmd.html#:autocmd

#### CoffeeBufNew

CoffeeBufNew is ran when a new scratch buffer is created. It's called from the
new buffer, so it can be used to do additional set up.

```vim
augroup CoffeeBufNew
  autocmd User * set wrap
augroup END
```

*Used By*: CoffeeCompile, CoffeeWatch, CoffeeRun

#### CoffeeBufUpdate

CoffeeBufUpdate is ran when a scratch buffer is updated with output from
`coffee`. It's called from the scratch buffer, so it can be used to alter the
compiled output.

```vim
" Switch back to the source buffer after updating.
augroup CoffeeBufUpdate
  autocmd User CoffeeCompile,CoffeeRun exec bufwinnr(b:coffee_src_buf) 'wincmd w'
augroup END
```

For example, to strip off the "Generated by" comment on the first line, put this
in your vimrc:

```vim
function! s:RemoveGeneratedBy()
  " If there was an error compiling, there's no comment to remove.
  if v:shell_error
    return
  endif

  " Save cursor position.
  let pos = getpos('.')

  " Remove first line.
  set modifiable
  1 delete _
  set nomodifiable

  " Restore cursor position.
  call setpos('.', pos)
endfunction

augroup CoffeeBufUpdate
  autocmd User CoffeeCompile,CoffeeWatch call s:RemoveGeneratedBy()
augroup END
```

*Used By*: CoffeeCompile, CoffeeWatch, CoffeeRun

## Configuration Variables

This is the full list of configuration variables available, with example
settings and default values. Use these in your vimrc to control the default
behavior.

#### coffee\_indent\_keep\_current

By default, the indent function matches the indent of the previous line if it
doesn't find a reason to indent or outdent. To change this behavior so it
instead keeps the [current indent of the cursor][98], use

    let coffee_indent_keep_current = 1

[98]: https://github.com/kchmck/vim-coffee-script/pull/98

*Default*: `unlet coffee_indent_keep_current`

Note that if you change this after a coffee file has been loaded, you'll have to
reload the indent script for the change to take effect:

    unlet b:did_indent | runtime indent/coffee.vim

#### coffee\_compiler

Path to the `coffee` executable used by the `Coffee` commands:

    let coffee_compiler = '/usr/bin/coffee'

*Default*: `'coffee'` (search `$PATH` for executable)

#### coffee\_make\_options

Options to pass to `coffee` with `:make`:

    let coffee_make_options = '--bare'

*Default*: `''` (nothing)

Note that `coffee_make_options` is embedded into `'makeprg'`, so `:compiler
coffee` must be ran after changing `coffee_make_options` for the changes to take
effect.

#### coffee\_cake

Path to the `cake` executable:

    let coffee_cake = '/opt/bin/cake'

*Default*: `'cake'` (search `$PATH` for executable)

#### coffee\_cake\_options

Options to pass to `cake` with `:make`:

    let coffee_cake_options = 'build'

*Default*: `''` (nothing)

#### coffee\_linter

Path to the `coffeelint` executable:

    let coffee_linter = '/opt/bin/coffeelint'

*Default*: `'coffeelint'` (search `$PATH` for executable)

#### coffee\_lint\_options

Options to pass to `coffeelint`:

    let coffee_lint_options = '-f lint.json'

*Default*: `''` (nothing)

#### coffee\_compile\_vert

Open the CoffeeCompile buffer with a vertical split instead of a horizontal
one:

    let coffee_compile_vert = 1

*Default*: `unlet coffee_compile_vert`

#### coffee\_watch\_vert

Open the CoffeeWatch buffer with a vertical split instead of a horizontal
one:

    let coffee_watch_vert = 1

*Default*: `unlet coffee_watch_vert`

#### coffee\_run\_vert

Open the CoffeeRun buffer with a vertical split instead of a horizontal
one:

    let coffee_run_vert = 1

*Default*: `unlet coffee_run_vert`

## Configure Syntax Highlighting

Add these lines to your vimrc to disable the relevant syntax group.

#### Disable trailing whitespace error

Trailing whitespace is highlighted as an error by default. This can be disabled
with:

    hi link coffeeSpaceError NONE

#### Disable trailing semicolon error

Trailing semicolons are considered an error (for help transitioning from
JavaScript.) This can be disabled with:

    hi link coffeeSemicolonError NONE

#### Disable reserved words error

Reserved words like `function` and `var` are highlighted as an error where
they're not allowed in CoffeeScript. This can be disabled with:

    hi link coffeeReservedError NONE

## Tune Vim for CoffeeScript

Changing these core settings can make vim more CoffeeScript friendly.

#### Fold by indentation

Folding by indentation works well for CoffeeScript functions and classes:

  ![Folding](http://i.imgur.com/gDgUBdO.png)

To fold by indentation in CoffeeScript files, add this line to your vimrc:

    autocmd BufNewFile,BufReadPost *.coffee setl foldmethod=indent nofoldenable

With this, folding is disabled by default but can be quickly toggled per-file
by hitting `zi`. To enable folding by default, remove `nofoldenable`:

    autocmd BufNewFile,BufReadPost *.coffee setl foldmethod=indent

#### Two-space indentation

To get standard two-space indentation in CoffeeScript files, add this line to
your vimrc:

    autocmd BufNewFile,BufReadPost *.coffee setl shiftwidth=2 expandtab
