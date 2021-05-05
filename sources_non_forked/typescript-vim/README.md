Typescript Syntax for Vim
=========================

Syntax file and other settings for [TypeScript](http://typescriptlang.org). The
syntax file is taken from this [blog
post](https://docs.microsoft.com/en-us/archive/blogs/interoperability/sublime-text-vi-emacs-typescript-enabled).

Checkout [Tsuquyomi](https://github.com/Quramy/tsuquyomi) for omni-completion
and other features for TypeScript editing.

Install
-------

From Vim 8 onward, the plugin can be installed as simply as (Unix/Mac):
```
git clone https://github.com/leafgarland/typescript-vim.git ~/.vim/pack/typescript/start/typescript-vim
```

On Windows/Powershell, use the following:
```
git clone https://github.com/leafgarland/typescript-vim.git $home/vimfiles/pack/typescript/start/typescript-vim
```

For older versions of Vim, the simplest way to install is via a Vim add-in manager such as
[Plug](https://github.com/junegunn/vim-plug),
[Vundle](https://github.com/gmarik/vundle) or
[Pathogen](https://github.com/tpope/vim-pathogen/).

_See the [Installation Wiki](https://github.com/leafgarland/typescript-vim/wiki/Installation)_

### Pathogen

```
git clone https://github.com/leafgarland/typescript-vim.git ~/.vim/bundle/typescript-vim
```

If you want to install manually then you need to copy the files from this
repository into your vim path, see the vim docs for [:help
runtimepath](http://vimdoc.sourceforge.net/htmldoc/options.html#'runtimepath')
for more information. This might be as simple as copying the files and
directories to `~/.vim/` but it depends on your Vim install and operating
system.

Usage
-----

Once the files are installed the syntax highlighting and other settings will be
automatically enabled anytime you edit a `.ts` file.

Indenting
---------

This plugin includes a custom indenter (based on [pangloss/vim-javascript's
indenter](https://github.com/pangloss/vim-javascript/blob/master/indent/javascript.vim)),
it works pretty well but there are cases where it fails. If these bother you or
want to use other indent settings you can disable it by setting a flag in your
`.vimrc`:

```vim
let g:typescript_indent_disable = 1
```

If you want the indenter to automatically indent chained method calls as you type.

```typescript
something
    .foo()
    .bar();
```

Then add something like `setlocal indentkeys+=0.` to your `.vimrc`, see `:help
'indentkeys'` in vim for more information.

If you use the `=` operator to re-indent code it will always indent
chained method calls - this can be disabled by changing the regex the
indent script uses to identify indented lines. In this case removing '.'
from the regex means that it wont indent lines starting with '.'. Note,
this is not ideal as the regex may change making your setting out of date.

```vim
let g:typescript_opfirst='\%([<>=,?^%|*/&]\|\([-:+]\)\1\@!\|!=\|in\%(stanceof\)\=\>\)'
```

Compiler settings
-----------------

This plugin contains compiler settings to set `makeprg` and `errorformat`. 
The compiler settings enable you to call the `tsc` compiler directly from Vim
and display any errors or warnings in Vim's QuickFix window. 

To run the compiler, enter `:make`, this will run `tsc` against the last saved
version of your currently edited file.

The default for `makeprg` is `tsc $* %`. You can enter other compiler options into your `:make`
command line and they will be inserted in place of `$*`.

There are options to change the compiler name and to insert default options.

```vim
let g:typescript_compiler_binary = 'tsc'
let g:typescript_compiler_options = ''
```

These options will be passed to the binary as command arguments. For example,
if `g:typescript_compiler_binary = 'tsc'` and `g:typescript_compiler_options = '--lib es6'`,
`l:makeprg` will be: `tsc --lib es6 $* %`.

You can completely override this plugin's compiler settings with something like
this in your `.vimrc`, where you can set makeprg to whatever you want.

```vim
  autocmd FileType typescript :set makeprg=tsc
```

Note, this plugin's compiler settings are not used by Syntastic which has its own
way of changing the options. See https://github.com/scrooloose/syntastic#faqargs.

You can use something like this in your `.vimrc` to make the QuickFix
window automatically appear if `:make` has any errors.

```vim
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
```

Syntax highlighting
-------------------

Syntax highlighting for TypeScript can be customized by following variables.

- `g:typescript_ignore_typescriptdoc`: When this variable is defined, doccomments will not be
  highlighted.
- `g:typescript_ignore_browserwords`: When this variable is set to `1`, browser API names such as
  `window` or `document` will not be highlighted. (default to `0`)

![Obligatory screenshot](https://raw.github.com/leafgarland/typescript-vim/master/vimshot01.png)
