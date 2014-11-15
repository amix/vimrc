# To-do list for the `notes.vim` plug-in

 * The note name highlighting uses word boundaries so that 'git' inside 'fugitive' is not highlighted, however this breaks highlighting of note names ending in punctuation (or more generically ending in non-word characters).
 * The `ftplugin/notes.vim` script used to clear the [matchpairs] [matchpairs] option so that pairs of characters are not highlighted in notes (the irrelevant highlighting was starting to annoy me). Several people have since complained that Vim rings a bell or flashes the screen for every key press in insert mode when editing notes. I've now removed the matchpairs manipulation from the plug-in but I suspect that this may actually be a bug in Vim; to be investigated. See also [issue 10 on GitHub] [issue_10].
 * Override `<F1>` to show a quick reference of available commands?
 * Define aliases of the available commands that start with `Note` (to help people getting started with the plug-in).
 * Add a key mapping to toggle text folding (currently in my `~/.vimrc`)
 * Add a key mapping or command to toggle the visibility of `{{{ … }}}` code markers?
 * Find a good way to support notes with generates contents, e.g. *'all notes'*.
 * When renaming a note, also update references to the note in other notes? (make this optional of course!)
 * Improve highlighting of lines below a line with a `DONE` marker; when navigating over such lines, the highlighting will sometimes disappear (except on the first line). See also [issue #2 on GitHub] [issue_2].

[issue_2]: https://github.com/xolox/vim-notes/issues/2
[issue_10]: https://github.com/xolox/vim-notes/issues/10
[matchpairs]: http://vimdoc.sourceforge.net/htmldoc/options.html#%27matchpairs%27
