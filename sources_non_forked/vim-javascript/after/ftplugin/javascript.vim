" Vim filetype plugin file
" Language:     JavaScript
" Maintainer:   vim-javascript community
" URL:          https://github.com/pangloss/vim-javascript

setlocal iskeyword+=$ suffixesadd+=.js

let b:undo_ftplugin .= ' | setlocal iskeyword< suffixesadd<'
