" Vim filetype plugin
" Language: Jade
" Maintainer: Joshua Borton
" Credits: Tim Pope

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

let s:save_cpo = &cpo
set cpo-=C

" Define some defaults in case the included ftplugins don't set them.
let s:undo_ftplugin = ""
let s:browsefilter = "All Files (*.*)\t*.*\n"
let s:match_words = ""

runtime! ftplugin/html.vim ftplugin/html_*.vim ftplugin/html/*.vim
unlet! b:did_ftplugin

" Override our defaults if these were set by an included ftplugin.
if exists("b:undo_ftplugin")
  let s:undo_ftplugin = b:undo_ftplugin
  unlet b:undo_ftplugin
endif
if exists("b:browsefilter")
  let s:browsefilter = b:browsefilter
  unlet b:browsefilter
endif
if exists("b:match_words")
  let s:match_words = b:match_words
  unlet b:match_words
endif

" Change the browse dialog on Win32 to show mainly Haml-related files
if has("gui_win32")
  let b:browsefilter="Jade Files (*.jade)\t*.jade\n" . s:browsefilter
endif

" Load the combined list of match_words for matchit.vim
if exists("loaded_matchit")
  let b:match_words = s:match_words
endif

setlocal comments=://-,:// commentstring=//\ %s

setlocal suffixesadd+=.jade

let b:undo_ftplugin = "setl cms< com< "
      \ " | unlet! b:browsefilter b:match_words | " . s:undo_ftplugin

let &cpo = s:save_cpo

" vim:set sw=2:
