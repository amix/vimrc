                   ,
                  / \,,_  .'|
               ,{{| /}}}}/_.'            _____________________________________________
              }}}}` '{{'  '.            /                                             \
            {{{{{    _   ;, \          /            Ladies and Gentlemen,              \
         ,}}}}}}    /o`\  ` ;)        |                                                |
        {{{{{{   /           (        |                 this is ...                    |
        }}}}}}   |            \       |                                                |
       {{{{{{{{   \            \      |                                                |
       }}}}}}}}}   '.__      _  |     |    _____             __             __  _      |
       {{{{{{{{       /`._  (_\ /     |   / ___/__  ______  / /_____ ______/ /_(_)____ |
        }}}}}}'      |    //___/   --=:   \__ \/ / / / __ \/ __/ __ `/ ___/ __/ / ___/ |
    jgs `{{{{`       |     '--'       |  ___/ / /_/ / / / / /_/ /_/ (__  ) /_/ / /__   |
         }}}`                         | /____/\__, /_/ /_/\__/\__,_/____/\__/_/\___/   |
                                      |      /____/                                    |
                                      |                                               /
                                       \_____________________________________________/


- - -
1\. [Introduction](#introduction)  
2\. [Installation](#installation)  
3\. [FAQ](#faq)  
4\. [Other resources](#otherresources)  
- - -

<a name="introduction"></a>

## 1\. Introduction

Syntastic is a syntax checking plugin for Vim that runs files through external
syntax checkers and displays any resulting errors to the user. This can be done
on demand, or automatically as files are saved. If syntax errors are detected,
the user is notified and is happy because they didn't have to compile their
code or execute their script to find them.

At the time of this writing, syntax checking plugins exist for ActionScript,
Ada, AppleScript, Arduino, AsciiDoc, ASM, BEMHTML, Bro, Bourne shell, C,
C++, C#, Cabal, Chef, CoffeeScript, Coco, Coq, CSS, Cucumber, CUDA, D, Dart,
DocBook, Dust, Elixir, Erlang, eRuby, Fortran, Gentoo metadata, GLSL, Go,
Haml, Haskell, Haxe, Handlebars, HSS, HTML, Java, JavaScript, JSON, JSX, LESS,
Lex, Limbo, LISP, LLVM intermediate language, Lua, MATLAB, NASM, Objective-C,
Objective-C++, OCaml, Perl, Perl POD, PHP, gettext Portable Object, OS X
and iOS property lists, Puppet, Python, Racket, R, reStructuredText, Ruby,
SASS/SCSS, Scala, Slim, Tcl, TeX, Texinfo, Twig, TypeScript, Vala, Verilog,
VHDL, VimL, xHtml, XML, XSLT, YACC, YAML, z80, Zope page templates, and zsh.
See the [wiki][3] for details about the corresponding supported checkers.

Below is a screenshot showing the methods that Syntastic uses to display syntax
errors.  Note that, in practise, you will only have a subset of these methods
enabled.

![Screenshot 1][0]

1. Errors are loaded into the location list for the corresponding window.
2. When the cursor is on a line containing an error, the error message is echoed in the command window.
3. Signs are placed beside lines with errors - note that warnings are displayed in a different color.
4. There is a configurable statusline flag you can include in your statusline config.
5. Hover the mouse over a line containing an error and the error message is displayed as a balloon.
6. (not shown) Highlighting errors with syntax highlighting. Erroneous parts of lines can be highlighted.

<a name="installation"></a>

## 2\. Installation

Installing syntastic is easy but first you need to have the [pathogen][1]
plugin installed.  If you already have [pathogen][1] working then skip
[Step 1](#step1) and go to [Step 2](#step2).


<a name="step1"></a>

### 2.1\. Step 1: Install pathogen.vim

First I'll show you how to install Tim Pope's [pathogen][1] so that it's easy to
install syntastic.  Do this in your terminal so that you get the `pathogen.vim`
file and the directories it needs:
```sh
mkdir -p ~/.vim/autoload ~/.vim/bundle && \
curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
```
Next you *need* to add this to your `~/.vimrc`:
```vim
execute pathogen#infect()
```

<a name="step2"></a>

### 2.2\. Step 2: Install syntastic as a pathogen bundle

You now have pathogen installed and can put syntastic into `~/.vim/bundle` like
this:
```sh
cd ~/.vim/bundle && \
git clone https://github.com/scrooloose/syntastic.git
```
Quit vim and start it back up to reload it, then type:
```vim
:Helptags
```
If you get an error when you do this, then you probably didn't install
[pathogen][1] right.  Go back to [Step 1](#step1) and make sure you did the following:

1. Created both the `~/.vim/autoload` and `~/.vim/bundle` directories.
2. Added the `call pathogen#infect()` line to your `~/.vimrc` file
3. Did the `git clone` of syntastic inside `~/.vim/bundle`
4. Have permissions to access all of these directories.


<a name="faq"></a>

## 3\. FAQ

<a name="faqinfo"></a>

__Q. I installed syntastic but it isn't reporting any errors...__

A. The most likely reason is that none of the syntax checkers that it requires
is installed. For example: by default, python requires either `flake8` or
`pylint` to be installed and in your `$PATH`. To see which executables are
supported, look at the [wiki][3]. Note that aliases do not work; the actual
executables must be available in your `$PATH`. Symbolic links are okay though.
You can see syntastic's idea of available checkers by running `:SyntasticInfo`.

Another reason it could fail is that either the command line options or the
error output for a syntax checker may have changed. In this case, make sure you
have the latest version of the syntax checker installed. If it still fails then
create an issue - or better yet, create a pull request.

<a name="faqperl"></a>

__Q. The `perl` checker has stopped working...__

A. The `perl` checker runs `perl -c` against your file, which in turn
__executes__ any `BEGIN`, `UNITCHECK`, and `CHECK` blocks, and any `use`
statements in your file (cf. [perlrun][10]).  This is probably fine if you
wrote the file yourself, but it's a security problem if you're checking third
party files.  Since there is currently no way to disable this behaviour while
still producing useful results, the checker is now disabled by default.  To
(re-)enable it, make sure the `g:syntastic_perl_checkers` list includes `perl`,
and set `g:syntastic_enable_perl_checker` to 1 in your vimrc:
```vim
let g:syntastic_enable_perl_checker = 1
```

<a name="faqrust"></a>

__Q. What happened to the `rustc` checker?__

A. It has been included in the [Rust compiler package][12].  If you have
a recent version of the Rust compiler, the checker should be picked up
automatically by syntastic.

<a name="faqloclist"></a>

__Q. I run a checker and the location list is not updated...__

A. By default the location list is changed only when you run the `:Errors`
command, in order to minimise conflicts with other plugins.  If you want the
location list to always be updated when you run the checkers, add this line to
your vimrc:
```vim
let g:syntastic_always_populate_loc_list = 1
```

<a name="faqargs"></a>

__Q. How can I pass additional arguments to a checker?__

A. Almost all syntax checkers use the `makeprgBuild()` function. Those checkers
that do can be configured using global variables. The general form of the
global `args` variables is `syntastic_<filetype>_<checker>_args`.

So, If you wanted to pass "--my --args --here" to the ruby mri checker you
would add this line to your vimrc:
```vim
let g:syntastic_ruby_mri_args = "--my --args --here"
```

See `:help syntastic-checker-options` for more information.

<a name="faqcheckers"></a>

__Q. Syntastic supports several checkers for my filetype - how do I tell it
which one(s) to use?__

A. Stick a line like this in your vimrc:
```vim
let g:syntastic_<filetype>_checkers = ['<checker-name>']
```

To see the list of supported checkers for your filetype look at the
[wiki][3].

e.g. Python has the following checkers, among others: `flake8`, `pyflakes`,
`pylint` and a native `python` checker.

To tell syntastic to use `pylint`, you would use this setting:
```vim
let g:syntastic_python_checkers = ['pylint']
```

Some filetypes, like PHP, have style checkers as well as syntax checkers. These
can be chained together like this:
```vim
let g:syntastic_php_checkers = ['php', 'phpcs', 'phpmd']
```

This is telling syntastic to run the `php` checker first, and if no errors are
found, run `phpcs`, and then `phpmd`.

You can also run checkers explicitly by calling `:SyntasticCheck <checker>`.

e.g. to run `phpcs` and `phpmd`:
```vim
:SyntasticCheck phpcs phpmd
```

This works for any checkers available for the current filetype, even if they
aren't listed in `g:syntastic_<filetype>_checkers`.  You can't run checkers for
"foreign" filetypes though (e.g. you can't run, say, a Python checker if the
current filetype is `php`).

<a name="faqaggregate"></a>

__Q. How can I display together the errors found by all checkers enabled for
the current file?__

A. Set `g:syntastic_aggregate_errors` to 1 in your vimrc:
```vim
let g:syntastic_aggregate_errors = 1
```

See `:help syntastic-aggregating-errors` for more details.

<a name="faqlnext"></a>

__Q. How can I jump between the different errors without using the location
list at the bottom of the window?__

A. Vim provides several built in commands for this. See `:help :lnext` and
`:help :lprev`.

If you use these commands a lot then you may want to add shortcut mappings to
your vimrc, or install something like [unimpaired][2], which provides such
mappings (among other things).

<a name="faqstyle"></a>

__Q. A syntax checker is giving me unwanted/strange style tips?__

A. Some filetypes (e.g. php) have style checkers as well as syntax
checkers. You can usually configure the options that are passed to the style
checkers, or just disable them. Take a look at the [wiki][3] to see what
options are available.

Alternatively, you can use `g:syntastic_quiet_messages` to filter out the
messages you don't want to see. e.g. To turn off all style messages:
```vim
let g:syntastic_quiet_messages = { "type": "style" }
```
See `:help syntastic_quiet_messages` for details.

<a name="faqbdelete"></a>

__Q. The error window is closed automatically when I :quit the current buffer
but not when I :bdelete it?__

A. There is no safe way to handle that situation automatically, but you can
work around it:

```vim
nnoremap <silent> <C-d> :lclose<CR>:bdelete<CR>
cabbrev <silent> bd lclose\|bdelete
```


<a name="otherresources"></a>

## 4\. Other resources

The preferred place for posting suggestions, reporting bugs, and general
discussions related to syntastic is the [issue tracker at GitHub][4].
A guide for writing syntax checkers can be found in the [wiki][11].
There are also a dedicated [google group][5], and a
[syntastic tag at StackOverflow][6].

Syntastic aims to provide a common interface to syntax checkers for as many
languages as possible.  For particular languages, there are, of course, other
plugins that provide more functionality than syntastic.  You might want to take
a look at [jedi-vim][7], [python-mode][8], or [YouCompleteMe][9].

[0]: https://github.com/scrooloose/syntastic/raw/master/_assets/screenshot_1.png
[1]: https://github.com/tpope/vim-pathogen
[2]: https://github.com/tpope/vim-unimpaired
[3]: https://github.com/scrooloose/syntastic/wiki/Syntax-Checkers
[4]: https://github.com/scrooloose/syntastic/issues
[5]: https://groups.google.com/group/vim-syntastic
[6]: http://stackoverflow.com/questions/tagged/syntastic
[7]: https://github.com/davidhalter/jedi-vim
[8]: https://github.com/klen/python-mode
[9]: http://valloric.github.io/YouCompleteMe/
[10]: http://perldoc.perl.org/perlrun.html#*-c*
[11]: https://github.com/scrooloose/syntastic/wiki/Syntax-Checker-Guide
[12]: https://github.com/rust-lang/rust/
