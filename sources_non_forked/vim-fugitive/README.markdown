# fugitive.vim

Fugitive is the premier Vim plugin for Git.  Or maybe it's the premier Git
plugin for Vim?  Either way, it's "so awesome, it should be illegal".  That's
why it's called Fugitive.

The crown jewel of Fugitive is `:Git` (or just `:G`), which calls any
arbitrary Git command.  If you know how to use Git at the command line, you
know how to use `:Git`.  It's vaguely akin to `:!git` but with numerous
improvements:

* The default behavior is to directly echo the command's output.  Quiet
  commands like `:Git add` avoid the dreaded "Press ENTER or type command to
  continue" prompt.
* `:Git commit`, `:Git rebase -i`, and other commands that invoke an editor do
  their editing in the current Vim instance.
* `:Git diff`, `:Git log`, and other verbose, paginated commands have their
  output loaded into a temporary buffer.  Force this behavior for any command
  with `:Git --paginate` or `:Git -p`.
* `:Git blame` uses a temporary buffer with maps for additional triage.  Press
  enter on a line to view the commit where the line changed, or `g?` to see
  other available maps.  Omit the filename argument and the currently edited
  file will be blamed in a vertical, scroll-bound split.
* `:Git mergetool` and `:Git difftool` load their changesets into the quickfix
  list.
* Called with no arguments, `:Git` opens a summary window with dirty files and
  unpushed and unpulled commits.  Press `g?` to bring up a list of maps for
  numerous operations including diffing, staging, committing, rebasing, and
  stashing.  (This is the successor to the old `:Gstatus`.)
* This command (along with all other commands) always uses the current
  buffer's repository, so you don't need to worry about the current working
  directory.

Additional commands are provided for higher level operations:

* View any blob, tree, commit, or tag in the repository with `:Gedit` (and
  `:Gsplit`, etc.).  For example, `:Gedit HEAD~3:%` loads the current file as
  it existed 3 commits ago.
* `:Gdiffsplit` (or `:Gvdiffsplit`) brings up the staged version of the file
  side by side with the working tree version.  Use Vim's diff handling
  capabilities to apply changes to the staged version, and write that buffer
  to stage the changes.  You can also give an arbitrary `:Gedit` argument to
  diff against older versions of the file.
* `:Gread` is a variant of `git checkout -- filename` that operates on the
  buffer rather than the file itself.  This means you can use `u` to undo it
  and you never get any warnings about the file changing outside Vim.
* `:Gwrite` writes to both the work tree and index versions of a file, making
  it like `git add` when called from a work tree file and like `git checkout`
  when called from the index or a blob in history.
* `:Ggrep` is `:grep` for `git grep`.  `:Glgrep` is `:lgrep` for the same.
* `:GMove` does a `git mv` on the current file and changes the buffer name to
  match.  `:GRename` does the same with a destination filename relative to the
  current file's directory.
* `:GDelete` does a `git rm` on the current file and simultaneously deletes
  the buffer.  `:GRemove` does the same but leaves the (now empty) buffer
  open.
* `:GBrowse` to open the current file on the web front-end of your favorite
  hosting provider, with optional line range (try it in visual mode).  Plugins
  are available for popular providers such as [GitHub][rhubarb.vim],
  [GitLab][fugitive-gitlab.vim], [Bitbucket][fubitive.vim],
  [Gitee][fugitive-gitee.vim], [Pagure][pagure],
  [Phabricator][vim-phabricator], [Azure DevOps][fugitive-azure-devops.vim],
  and [sourcehut][srht.vim].

[rhubarb.vim]: https://github.com/tpope/vim-rhubarb
[fugitive-gitlab.vim]: https://github.com/shumphrey/fugitive-gitlab.vim
[fubitive.vim]: https://github.com/tommcdo/vim-fubitive
[fugitive-gitee.vim]: https://github.com/linuxsuren/fugitive-gitee.vim
[pagure]: https://github.com/FrostyX/vim-fugitive-pagure
[vim-phabricator]: https://github.com/jparise/vim-phabricator
[fugitive-azure-devops.vim]: https://github.com/cedarbaum/fugitive-azure-devops.vim
[srht.vim]: https://git.sr.ht/~willdurand/srht.vim

Add `%{FugitiveStatusline()}` to `'statusline'` to get an indicator
with the current branch in your statusline.

For more information, see `:help fugitive`.

## Screencasts

* [A complement to command line git](http://vimcasts.org/e/31)
* [Working with the git index](http://vimcasts.org/e/32)
* [Resolving merge conflicts with vimdiff](http://vimcasts.org/e/33)
* [Browsing the git object database](http://vimcasts.org/e/34)
* [Exploring the history of a git repository](http://vimcasts.org/e/35)

## Installation

Install using your favorite package manager, or use Vim's built-in package
support:

    mkdir -p ~/.vim/pack/tpope/start
    cd ~/.vim/pack/tpope/start
    git clone https://tpope.io/vim/fugitive.git
    vim -u NONE -c "helptags fugitive/doc" -c q

## FAQ

> What happened to the dispatch.vim backed asynchronous `:Gpush` and
> `:Gfetch`?

This behavior was divisive, confusing, and complicated inputting passwords, so
it was removed.  Use `:Git! push` to use Fugitive's own asynchronous
execution, or retroactively make `:Git push` asynchronous by pressing
`CTRL-D`.

> Why am I getting `core.worktree is required when using an external Git dir`?

Git generally sets `core.worktree` for you automatically when necessary, but
if you're doing something weird, or using a third-party tool that does
something weird, you may need to set it manually:

    git config core.worktree "$PWD"

This may be necessary even when simple `git` commands seem to work fine
without it.

> So I have a symlink and...

Stop.  Just stop.  If Git won't deal with your symlink, then Fugitive won't
either.  Consider using a [plugin that resolves
symlinks](https://github.com/aymericbeaumet/symlink.vim), or even better,
using fewer symlinks.

## Self-Promotion

Like fugitive.vim? Follow the repository on
[GitHub](https://github.com/tpope/vim-fugitive) and vote for it on
[vim.org](http://www.vim.org/scripts/script.php?script_id=2975).  And if
you're feeling especially charitable, follow [tpope](http://tpo.pe/) on
[Twitter](http://twitter.com/tpope) and
[GitHub](https://github.com/tpope).

## License

Copyright (c) Tim Pope.  Distributed under the same terms as Vim itself.
See `:help license`.
