# ack.vim #

This plugin is a front for the Perl module
[App::Ack](http://search.cpan.org/~petdance/ack/ack).  Ack can be used as a
replacement for 99% of the uses of _grep_.  This plugin will allow you to run
ack from vim, and shows the results in a split window.

The *Official Version* of this plugin is available at [vim.org](http://www.vim.org/scripts/script.php?script_id=2572).

## Installation ##


### Ack

You have to install [ack](http://betterthangrep.com/), of course.

Install on Debian / Ubuntu with:

    sudo apt-get install ack-grep

For Debian / Ubuntu you can add this line into your .vimrc:

    let g:ackprg="ack-grep -H --nocolor --nogroup --column"

Install on Gentoo with:

    sudo emerge ack

Install with Homebrew:

    brew install ack

Install with MacPorts:

    sudo port install p5-app-ack

Install with Gentoo Prefix

    emerge ack

Otherwise, you are on your own.

### The Plugin

If you have [Rake](http://rake.rubyforge.org/) installed, you can just run: `rake install`.

Otherwise, the file ack.vim goes in ~/.vim/plugin, and the ack.txt file belongs in ~/.vim/doc.  Be sure to run

    :helptags ~/.vim/doc

afterwards.


## Usage ##

    :Ack [options] {pattern} [{directory}]

Search recursively in {directory} (which defaults to the current directory) for the {pattern}.

Files containing the search term will be listed in the split window, along with
the line number of the occurrence, once for each occurrence.  [Enter] on a line
in this window will open the file, and place the cursor on the matching line.

Just like where you use :grep, :grepadd, :lgrep, and :lgrepadd, you can use `:Ack`, `:AckAdd`, `:LAck`, and `:LAckAdd` respectively. (See `doc/ack.txt`, or install and `:h Ack` for more information.)

**From the [ack docs](http://betterthangrep.com/)** (my favorite feature):

    --type=TYPE, --type=noTYPE

        Specify the types of files to include or exclude from a search. TYPE is a filetype, like perl or xml. --type=perl can also be specified as --perl, and --type=noperl can be done as --noperl.

        If a file is of both type "foo" and "bar", specifying --foo and --nobar will exclude the file, because an exclusion takes precedence over an inclusion.

        Type specifications can be repeated and are ORed together.

        See ack --help=types for a list of valid types.

### Keyboard Shortcuts ###

In the quickfix window, you can use:

    o    to open (same as enter)
    go   to preview file (open but maintain focus on ack.vim results)
    t    to open in new tab
    T    to open in new tab silently
    v    to open in vertical split
    gv   to open in vertical split silently
    q    to close the quickfix window

This Vim plugin is derived (and by derived, I mean copied, essentially) from
Antoine Imbert's blog post [Ack and Vim
Integration](http://blog.ant0ine.com/typepad/2007/03/ack-and-vim-integration.html) (in
particular, the function at the bottom of the post).  I added a help file that
provides just enough reference to get you going.  I also highly recommend you
check out the docs for the Perl script 'ack', for obvious reasons: [ack -
grep-like text finder](http://betterthangrep.com/).
