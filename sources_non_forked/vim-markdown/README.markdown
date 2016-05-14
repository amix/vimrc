# Vim Markdown runtime files

This is the development version of Vim's included syntax highlighting and
filetype plugins for Markdown.  Generally you don't need to install these if
you are running a recent version of Vim.

One difference between this repository and the upstream files in Vim is that
the former forces `*.md` as Markdown, while the latter detects it as Modula-2,
with an exception for `README.md`.  If you'd like to force Markdown without
installing from this repository, add the following to your vimrc:

    autocmd BufNewFile,BufReadPost *.md set filetype=markdown

If you want to enable fenced code block syntax highlighting in your markdown
documents you can enable it in your `.vimrc` like so:

    let g:markdown_fenced_languages = ['html', 'python', 'bash=sh']

To disable markdown syntax concealing add the following to your vimrc:

    let g:markdown_syntax_conceal = 0

## License

Copyright © Tim Pope.  Distributed under the same terms as Vim itself.
See `:help license`.
