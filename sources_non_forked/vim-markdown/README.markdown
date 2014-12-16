# Vim Markdown runtime files

This is the development version of Vim's included syntax highlighting and
filetype plugins for Markdown.  Generally you don't need to install these if
you are running a recent version of Vim.

One difference between this repository and the upstream files in Vim is that
the former forces `*.md` as Markdown, while the latter detects it as Modula-2,
with an exception for `README.md`.  If you'd like to force Markdown without
installing from this repository, add the following to your vimrc:

    autocmd BufNewFile,BufReadPost *.md set filetype=markdown

## License

Copyright © Tim Pope.  Distributed under the same terms as Vim itself.
See `:help license`.
