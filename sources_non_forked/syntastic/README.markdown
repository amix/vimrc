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
1. [Introduction](#introduction)  
2. [Installation](#installation)  
2.1. [Requirements](#requirements)  
2.2. [Installing syntastic with Pathogen](#installpathogen)  
3. [Recommended settings](#settings)  
4. [FAQ](#faq)  
4.1. [I installed syntastic but it isn't reporting any errors...](#faqinfo)  
4.2. [The `python` checker complains about syntactically valid Python 3 constructs...](#faqpython3)  
4.3. [Are there any local checkers for HTML5 that I can use with syntastic?](#faqhtml5)  
4.4. [The `perl` checker has stopped working...](#faqperl)  
4.5. [What happened to the `rustc` checker?](#faqrust)  
4.6. [What happened to the `xcrun` checker?](#faqxcrun)  
4.7. [I run a checker and the location list is not updated...](#faqloclist)  
4.7. [I run`:lopen` or `:lwindow` and the error window is empty...](#faqloclist)  
4.8. [How can I pass additional arguments to a checker?](#faqargs)  
4.9. [Syntastic supports several checkers for my filetype - how do I tell which one(s) to use?](#faqcheckers)  
4.10. [What is the difference between syntax checkers and style checkers?](#faqstyle)  
4.11. [I have enabled multiple checkers for the current filetype. How can I display all errors from all checkers together?](#faqaggregate)  
4.12. [How can I jump between the different errors without using the location list at the bottom of the window?](#faqlnext)  
4.13. [The error window is closed automatically when I :quit the current buffer but not when I :bdelete it?](#faqbdelete)  
5. [Resources](#otherresources)  

- - -

<a name="introduction"></a>

## 1\. Introduction

Syntastic is a syntax checking plugin for [Vim][13] that runs files through
external syntax checkers and displays any resulting errors to the user. This
can be done on demand, or automatically as files are saved. If syntax errors
are detected, the user is notified and is happy because they didn't have to
compile their code or execute their script to find them.

At the time of this writing, syntastic has checking plugins for ActionScript,
Ada, Ansible configurations, API Blueprint, AppleScript, AsciiDoc, ASM,
BEMHTML, Bro, Bourne shell, C, C++, C#, Cabal, Chef, CoffeeScript, Coco,
Coq, CSS, Cucumber, CUDA, D, Dart, DocBook, Dockerfile, Dust, Elixir,
Erlang, eRuby, Fortran, Gentoo metadata, GLSL, Go, Haml, Haskell, Haxe,
Handlebars, HSS, HTML, Java, JavaScript, JSON, JSX, LESS, Lex, Limbo, LISP,
LLVM intermediate language, Lua, Markdown, MATLAB, Mercury, NASM, Nix,
Objective-C, Objective-C++, OCaml, Perl, Perl POD, PHP, gettext Portable
Object, OS X and iOS property lists, Pug (formerly Jade), Puppet, Python, QML,
R, Racket, Relax NG, reStructuredText, RPM spec, Ruby, SASS/SCSS, Scala, Slim,
SML, Sphinx, SQL, Stylus, Tcl, TeX, Texinfo, Twig, TypeScript, Vala, Verilog,
VHDL, VimL, xHtml, XML, XSLT, XQuery, YACC, YAML, z80, Zope page templates, and
zsh. See the [wiki][3] for details about the corresponding supported checkers.

A number of third-party Vim plugins also provide checkers for syntastic,
for example: [merlin][30], [omnisharp-vim][25], [rust.vim][12],
[syntastic-extras][26], [syntastic-more][27], [vim-crystal][29],
[vim-eastwood][28], and [vim-swift][24].

Below is a screenshot showing the methods that Syntastic uses to display syntax
errors. Note that, in practise, you will only have a subset of these methods
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

<a name="requirements"></a>

### 2.1\. Requirements

Syntastic itself has rather relaxed requirements: it doesn't have any external
dependencies, and it needs a version of [Vim][13] compiled with a few common
features: `autocmd`, `eval`, `file_in_path`, `modify_fname`, `quickfix`,
`reltime`, and `user_commands`. Not all possible combinations of features that
include the ones above make equal sense on all operating systems, but Vim
version 7 or later with the "normal", "big", or "huge" feature sets should be
fine.

Syntastic should work with any modern plugin managers for Vim, such as
[NeoBundle][14], [Pathogen][1], [Vim-Addon-Manager][15], [Vim-Plug][16], or
[Vundle][17]. Instructions for installing syntastic with [Pathogen][1] are
included below for completeness.

Starting with Vim version 7.4.1486 you can also load syntastic using the
standard mechanism of packages, without the help of third-party plugin managers
(see `:help packages` in Vim for details). Beware however that, while support
for packages has been added in Vim 7.4.1384, the functionality needed by
syntastic is present only in versions 7.4.1486 and later.

Last but not least: syntastic doesn't know how to do any syntax checks by
itself. In order to get meaningful results you need to install external
checkers corresponding to the types of files you use. Please consult the
[wiki][3] for a list of supported checkers.

<a name="installpathogen"></a>

### 2.2\. Installing syntastic with Pathogen

If you already have [Pathogen][1] working then skip [Step 1](#step1) and go to
[Step 2](#step2).

<a name="step1"></a>

#### 2.2.1\. Step 1: Install pathogen.vim

First I'll show you how to install Tim Pope's [Pathogen][1] so that it's easy to
install syntastic. Do this in your terminal so that you get the `pathogen.vim`
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

#### 2.2.2\. Step 2: Install syntastic as a Pathogen bundle

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
[Pathogen][1] right. Go back to [Step 1](#step1) and make sure you did the
following:

1. Created both the `~/.vim/autoload` and `~/.vim/bundle` directories.
2. Added the `execute pathogen#infect()` line to your `~/.vimrc` file
3. Did the `git clone` of syntastic inside `~/.vim/bundle`
4. Have permissions to access all of these directories.

<a name="settings"></a>

## 3\. Recommended settings

Syntastic has numerous options that can be configured, and the defaults
are not particularly well suitable for new users. It is recommended
that you start by adding the following lines to your `vimrc` file, and
return to them after reading the manual (see `:help syntastic` in Vim):
```vim
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
```

<a name="faq"></a>

## 4\. FAQ

<a name="faqinfo"></a>

__4.1. Q. I installed syntastic but it isn't reporting any errors...__

A. The most likely reason is that none of the syntax checkers that it requires
is installed. For example: by default, python requires either `flake8` or
`pylint` to be installed and in your `$PATH`. To see which executables are
supported, look at the [wiki][3]. Note that aliases do not work; the actual
executables must be available in your `$PATH`. Symbolic links are okay though.
You can see syntastic's idea of available checkers by running `:SyntasticInfo`.

A second probable reason is that none of the available checkers are
enabled. Syntastic comes preconfigured with a default list of enabled checkers
per filetype, but this list is kept short in order to prevent slowing down Vim
or trying to run conflicting checks. The command `:SyntasticInfo` will show you
which checkers are enabled. You can tell syntastic which checkers (among the
available ones) you want to run by setting `g:syntastic_<filetype>_checkers` in
your `vimrc` (see [below](#faqcheckers)).

A third possible reason is that the `$PATH` seen by syntastic might not be same
as the `$PATH` in your login shell. Syntastic runs checkers using the shell
pointed to by Vim's `shell` (or by `g:syntastic_shell`, if set), and that's the
shell you need to configure to set the proper `$PATH` and environment variables
for your checkers. You can see syntastic's idea of `$PATH` by running
```vim
:echo syntastic#util#system('echo "$PATH"')
```
on UNIX and Mac OS-X systems, or
```vim
:echo syntastic#util#system('echo %PATH%')
```
on Windows.

Finally, another reason it could fail is that either the command line options
or the error output for a syntax checker may have changed. In this case, make
sure you have the latest version of the syntax checker installed. If it still
fails then post an [issue][4] - or better yet, create a pull request.

<a name="faqpython3"></a>

__4.2. Q. The `python` checker complains about syntactically valid Python 3 constructs...__

A. Configure the `python` checker to call a Python 3 interpreter rather than
Python 2, e.g:
```vim
let g:syntastic_python_python_exec = '/path/to/python3'
```

<a name="faqhtml5"></a>

__4.3. Q. Are there any local checkers for HTML5 that I can use with syntastic?__

[HTML Tidy][18] has a fork named [HTML Tidy for HTML5][19]. It's a drop
in replacement, and syntastic can use it without changes. Just install it
somewhere and point `g:syntastic_html_tidy_exec` to its executable:
```vim
let g:syntastic_html_tidy_exec = 'tidy5'
```
Alternatively, you can install [vnu.jar][21] from the [validator.nu][20]
project and run it as a [HTTP server][23]:
```sh
$ java -Xss512k -cp /path/to/vnu.jar nu.validator.servlet.Main 8888
```
Then you can [configure][22] syntastic to use it:
```vim
let g:syntastic_html_validator_api = 'http://localhost:8888/'
```

<a name="faqperl"></a>

__4.4. Q. The `perl` checker has stopped working...__

A. The `perl` checker runs `perl -c` against your file, which in turn
__executes__ any `BEGIN`, `UNITCHECK`, and `CHECK` blocks, and any `use`
statements in your file (cf. [perlrun][10]). This is probably fine if you
wrote the file yourself, but it's a security problem if you're checking
third-party files. Since there is currently no way to disable this behaviour
while still producing useful results, the checker is now disabled by default.
To (re-)enable it, make sure the `g:syntastic_perl_checkers` list includes
`perl`, and set `g:syntastic_enable_perl_checker` to 1 in your `vimrc`:
```vim
let g:syntastic_enable_perl_checker = 1
```

<a name="faqrust"></a>

__4.5. Q. What happened to the `rustc` checker?__

A. It is now part of the [rust.vim][12] plugin. If you install this plugin the
checker should be picked up automatically by syntastic.

<a name="faqxcrun"></a>

__4.6. Q. What happened to the `xcrun` checker?__

A. The `xcrun` checker used to have a security problem and it has been removed.
A better checker for __Swift__ is part of the [vim-swift][24] plugin. If you
install this plugin the checker should be picked up automatically by syntastic.

<a name="faqloclist"></a>

__4.7. Q. I run a checker and the location list is not updated...__  
__4.7. Q. I run`:lopen` or `:lwindow` and the error window is empty...__

A. By default the location list is changed only when you run the `:Errors`
command, in order to minimise conflicts with other plugins. If you want the
location list to always be updated when you run the checkers, add this line to
your `vimrc`:
```vim
let g:syntastic_always_populate_loc_list = 1
```

<a name="faqargs"></a>

__4.8. Q. How can I pass additional arguments to a checker?__

A. Almost all syntax checkers use the `makeprgBuild()` function. Those checkers
that do can be configured using global variables. The general form of the
global `args` variables is `syntastic_<filetype>_<checker>_args`.

So, If you wanted to pass `--my --args --here` to the ruby mri checker you
would add this line to your `vimrc`:
```vim
let g:syntastic_ruby_mri_args = "--my --args --here"
```

See `:help syntastic-checker-options` for more information.

<a name="faqcheckers"></a>

__4.9. Q. Syntastic supports several checkers for my filetype - how do I tell it
which one(s) to use?__

A. Stick a line like this in your `vimrc`:
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

Checkers can be chained together like this:
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
filetype of the current file is `php`).

<a name="faqstyle"></a>

__4.10. Q. What is the difference between syntax checkers and style checkers?__

A. The errors and warnings they produce are highlighted differently and can
be filtered by different rules, but otherwise the distinction is pretty much
arbitrary. There is an ongoing effort to keep things consistent, so you can
_generally_ expect messages produced by syntax checkers to be _mostly_ related
to syntax, and messages produced by style checkers to be _mostly_ about style.
But there can be no formal guarantee that, say, a style checker that runs into
a syntax error wouldn't die with a fatal message, nor that a syntax checker
wouldn't give you warnings against using some constructs as being bad practice.
There is also no guarantee that messages marked as "style" are less severe than
the ones marked as "syntax" (whatever that might mean). And there are even a
few Frankenstein checkers (for example `flake8` and `pylama`) that, by their
nature, produce both kinds of messages. Syntastic is not smart enough to be
able to sort out these things by itself.

In fact it's more useful to look at this from the perspective of filtering
unwanted messages, rather than as an indicator of severity levels. The
distinction between syntax and style is orthogonal to the distinction between
errors and warnings, and thus you can turn off messages based on level, on
type, or both.

e.g. To disable all style messages:
```vim
let g:syntastic_quiet_messages = { "type": "style" }
```
See `:help syntastic_quiet_messages` for details.

<a name="faqaggregate"></a>

__4.11. Q. I have enabled multiple checkers for the current filetype. How can I
display all errors from all checkers together?__

A. Set `g:syntastic_aggregate_errors` to 1 in your `vimrc`:
```vim
let g:syntastic_aggregate_errors = 1
```

See `:help syntastic-aggregating-errors` for more details.

<a name="faqlnext"></a>

__4.12. Q. How can I jump between the different errors without using the location
list at the bottom of the window?__

A. Vim provides several built-in commands for this. See `:help :lnext` and
`:help :lprevious`.

If you use these commands a lot then you may want to add shortcut mappings to
your `vimrc`, or install something like [unimpaired][2], which provides such
mappings (among other things).

<a name="faqbdelete"></a>

__4.13. Q. The error window is closed automatically when I :quit the current buffer
but not when I :bdelete it?__

A. There is no safe way to handle that situation automatically, but you can
work around it:

```vim
nnoremap <silent> <C-d> :lclose<CR>:bdelete<CR>
cabbrev <silent> bd <C-r>=(getcmdtype()==#':' && getcmdpos()==1 ? 'lclose\|bdelete' : 'bd')<CR>
```

<a name="otherresources"></a>

## 5\. Resources

The preferred place for posting suggestions, reporting bugs, and general
discussions related to syntastic is the [issue tracker at GitHub][4].
A guide for writing syntax checkers can be found in the [wiki][11].
There are also a dedicated [google group][5], and a
[syntastic tag at StackOverflow][6].

Syntastic aims to provide a common interface to syntax checkers for as many
languages as possible. For particular languages, there are, of course, other
plugins that provide more functionality than syntastic. You might want to take
a look at [ghcmod-vim][31], [jedi-vim][7], [python-mode][8], [vim-go][32], or
[YouCompleteMe][9].

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
[12]: https://github.com/rust-lang/rust.vim
[13]: http://www.vim.org/
[14]: https://github.com/Shougo/neobundle.vim
[15]: https://github.com/MarcWeber/vim-addon-manager
[16]: https://github.com/junegunn/vim-plug/
[17]: https://github.com/gmarik/Vundle.vim
[18]: http://tidy.sourceforge.net/
[19]: http://www.htacg.org/tidy-html5/
[20]: http://about.validator.nu/
[21]: https://github.com/validator/validator/releases/latest
[22]: https://github.com/scrooloose/syntastic/wiki/HTML%3A---validator
[23]: http://validator.github.io/validator/#standalone
[24]: https://github.com/kballard/vim-swift
[25]: https://github.com/OmniSharp/omnisharp-vim
[26]: https://github.com/myint/syntastic-extras
[27]: https://github.com/roktas/syntastic-more
[28]: https://github.com/venantius/vim-eastwood
[29]: https://github.com/rhysd/vim-crystal
[30]: https://github.com/the-lambda-church/merlin
[31]: https://github.com/eagletmt/ghcmod-vim
[32]: https://github.com/fatih/vim-go

<!--
vim:tw=79:sw=4:
-->
