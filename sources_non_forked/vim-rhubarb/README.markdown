# rhubarb.vim

If [fugitive.vim][] is the Git, rhubarb.vim is the Hub.  Here's the full list
of features:

* Enables `:GBrowse` from fugitive.vim to open GitHub URLs.

* In commit messages, GitHub issues, issue URLs, and collaborators can be
  omni-completed (`<C-X><C-O>`, see `:help compl-omni`).  This makes inserting
  those `Closes #123` remarks slightly easier than copying and pasting from
  the browser.

[fugitive.vim]: https://github.com/tpope/vim-fugitive

## Installation

If you don't have a preferred installation method, I recommend
installing [pathogen.vim](https://github.com/tpope/vim-pathogen), and
then simply copy and paste:

    cd ~/.vim/bundle
    git clone https://github.com/tpope/vim-rhubarb.git
    vim -u NONE -c "helptags vim-rhubarb/doc" -c q

You'll also need [fugitive.vim][].

[Curl](http://curl.haxx.se/) (included with macOS) is required for features
that use the GitHub API (i.e., `:GBrowse` doesn't need it).
[Generate a personal access token](https://github.com/settings/tokens/new)
with repo permissions and add it to your `.netrc`:

    echo 'machine api.github.com login <user> password <token>' >> ~/.netrc

If you are using GitHub Enterprise, repeat this step for each domain (omit the
`api.` portion). You'll also need to tell Rhubarb the root URLs:

    let g:github_enterprise_urls = ['https://example.com']

## FAQ

> How do I turn off that preview window that shows the issue body?

    set completeopt-=preview

> What happened to the support for [`hub`](https://github.com/github/hub)?

Support was dropped partially because [GitHub CLI](https://github.com/cli/cli)
appears to be unseating it as the preferred GitHub command line solution, and
partially because `hub` isn't quite a perfect drop-in replacement for `git`,
making life more difficult for Fugitive.  My recommended solution is to call
it via a Git alias:

    git config --global alias.hub '!hub'

This will let you call `hub pull-request` via `:Git hub pull-request`, for
example.

## Self-Promotion

Like rhubarb.vim? Follow the repository on
[GitHub](https://github.com/tpope/vim-rhubarb).  And if
you're feeling especially charitable, follow [tpope](http://tpo.pe/) on
[Twitter](http://twitter.com/tpope) and
[GitHub](https://github.com/tpope).
