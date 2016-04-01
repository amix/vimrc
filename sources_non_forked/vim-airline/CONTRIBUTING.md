# Contributions

Contributions and pull requests are welcome.  Please take note of the following guidelines:

*  Adhere to the existing style as much as possible; notably, 2 space indents and long-form keywords.
*  Keep the history clean!  Squash your branches before you submit a pull request.  `pull --rebase` is your friend.
*  Any changes to the core should be tested against Vim 7.2.

# Bugs

Tracking down bugs can take a very long time due to different configurations, versions, and operating systems.  To ensure a timely response, please help me out by doing the following:

* Reproduce it with this [minivimrc][7] repository to rule out any configuration conflicts.  Even better, create a `gist` of your vimrc that is compatible with [pathogen][11].
* And to make it easier to reproduce, please supply the following:
  * the `:version` of vim
  * the commit of vim-airline you're using
  * the OS that you're using, including terminal emulator, GUI vs non-GUI

# Themes

*  If you submit a theme, please create a screenshot so it can be added to the [Wiki][14].
*  In the majority of cases, modifications to colors of existing themes will likely be rejected.  Themes are a subjective thing, so while you may prefer that a particular color be darker, another user will prefer it to be lighter, or something entirely different.  The more popular the theme, the more unlikely the change will be accepted.  However, it's pretty simple to create your own theme; copy the theme to `~/.vim/autoload/airline/themes` under a new name with your modifications, and it can be used.

# Maintenance

If you would like to take a more active role in improving vim-airline, please consider [becoming a maintainer][43].


[7]: https://github.com/bling/minivimrc
[11]: https://github.com/tpope/vim-pathogen
[14]: https://github.com/vim-airline/vim-airline/wiki/Screenshots
[43]: https://github.com/vim-airline/vim-airline/wiki/Becoming-a-Maintainer
