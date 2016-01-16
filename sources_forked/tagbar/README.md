# Tagbar: a class outline viewer for Vim

## What Tagbar is

Tagbar is a Vim plugin that provides an easy way to browse the tags of the
current file and get an overview of its structure. It does this by creating a
sidebar that displays the ctags-generated tags of the current file, ordered by
their scope. This means that for example methods in C++ are displayed under
the class they are defined in.

## What Tagbar is not

Tagbar is not a general-purpose tool for managing `tags` files. It only
creates the tags it needs on-the-fly in-memory without creating any files.
`tags` file management is provided by other plugins, like for example
[easytags](https://github.com/xolox/vim-easytags).

## Dependencies

[Vim 7.0](http://www.vim.org/) (But see note below)  
[Exuberant ctags 5.5](http://ctags.sourceforge.net/)

## Installation

Extract the archive or clone the repository into a directory in your
`'runtimepath'`, or use a plugin manager of your choice like
[pathogen](https://github.com/tpope/vim-pathogen). Don't forget to run
`:helptags` if your plugin manager doesn't do it for you so you can access the
documentation with `:help tagbar`.

Note: Vim versions < 7.0.167 have a bug that prevents Tagbar from working. If
you are affected by this use this alternate Tagbar download instead:
[zip](https://github.com/majutsushi/tagbar/zipball/70fix). It is on par with
version 2.2 but probably won't be updated after that due to the amount of
changes required.

If the ctags executable is not installed in one of the directories in your
`$PATH` environment variable you have to set the `g:tagbar_ctags_bin`
variable, see the documentation for more info.

## Quickstart

Put something like the following into your ~/.vimrc:

```vim
nmap <F8> :TagbarToggle<CR>
```

If you do this the F8 key will toggle the Tagbar window. You can of course use
any shortcut you want. For more flexible ways to open and close the window
(and the rest of the functionality) see the documentation.

## Support for additional filetypes

For filetypes that are not supported by Exuberant Ctags check out [the
wiki](https://github.com/majutsushi/tagbar/wiki) to see whether other projects
offer support for them and how to use them. Please add any other
projects/configurations that you find or create yourself so that others can
benefit from them, too.

## Note: If the file structure display is wrong

If you notice that there are some errors in the way your file's structure is
displayed in Tagbar, please make sure that the bug is actually in Tagbar
before you report an issue. Since Tagbar uses
[exuberant-ctags](http://ctags.sourceforge.net/) and compatible programs to do
the actual file parsing, it is likely that the bug is actually in the program
responsible for that filetype instead.

There is an example in `:h tagbar-issues` about how to run ctags manually so
you can determine where the bug actually is. If the bug is actually in ctags,
please report it on their website instead, as there is nothing I can do about
it in Tagbar. Thank you!

You can also have a look at [ctags bugs that have previously been filed
against Tagbar](https://github.com/majutsushi/tagbar/issues?labels=ctags-bug&page=1&state=closed).

## Screenshots

![screenshot1](https://i.imgur.com/Sf9Ls2r.png)
![screenshot2](https://i.imgur.com/n4bpPv3.png)

## License

Vim license, see LICENSE

## Maintainer

Jan Larres <[jan@majutsushi.net](mailto:jan@majutsushi.net)>
