# marked.vim

Open the current Markdown buffer in [Marked.app](http://markedapp.com/).

## Usage

This plugin adds the following commands to Markdown buffers:

    :MarkedOpen[!] Open the current Markdown buffer in Marked.app.
                   Call with a bang to prevent Marked.app from stealing
                   focus from Vim.

    :MarkedQuit    Close the current Markdown buffer in Marked.app.
                   Quit Marked.app if no other documents are open.

If you run `:MarkedOpen`, the document in Marked.app will be automatically
closed when Vim exists, and Marked.app will quit if no other documents are
open.

## License

Same as Vim itself, see `:help license`.
