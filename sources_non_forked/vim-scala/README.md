vim-scala
=========

This is a "bundle" for Vim that builds off of the initial Scala plugin modules
by Stefan Matthias Aust and adds some more "stuff" that I find useful, including
all of my notes and customizations.

## Installation

You really should be using Tim Pope's [Pathogen](https://github.com/tpope/vim-pathogen) module for Vim (http://tammersaleh.com/posts/the-modern-vim-config-with-pathogen) if you're going to clone this repository because, well... you should.

### Using the command-line

Using wget:

```mkdir -p ~/.vim/{ftdetect,indent,syntax} && for d in ftdetect indent syntax ; do wget -O ~/.vim/$d/scala.vim https://raw.githubusercontent.com/derekwyatt/vim-scala/master/$d/scala.vim; done```

Using cURL:

```mkdir -p ~/.vim/{ftdetect,indent,syntax} && for d in ftdetect indent syntax ; do curl -o ~/.vim/$d/scala.vim https://raw.githubusercontent.com/derekwyatt/vim-scala/master/$d/scala.vim; done```

### Vundle
Alternatively, you can use [Vundle](https://github.com/gmarik/vundle) to
manage your plugins.

If you have Vundle installed, simply add the following to your .vimrc:

```vim
Plugin 'derekwyatt/vim-scala'
```

and then run

```vim
:PluginInstall
```

to install it.

## Sorting of import statements
    :SortScalaImports

There are different modes for import sorting available. For details, please
consult the vimdoc help with

    :help :SortScalaImports

## Scaladoc comment indentation

By default, the plugin indents documentation comments according to the standard
Javadoc format

    /**
     * This is a doc comment using Javadoc-style indentation.
     */

To enable the indentation standard as recommended for Scaladoc comments (from
http://docs.scala-lang.org/style/scaladoc.html, since Scaladoc2), add the
command ``let g:scala_scaladoc_indent = 1`` to .vimrc file, e.g:

    /** This is a Scaladoc comment using the recommended indentation.
      * let g:scala_scaladoc_indent = 1
      */
