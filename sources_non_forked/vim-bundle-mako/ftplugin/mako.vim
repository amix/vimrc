" Vim filetype plugin file
" Language:     Mako
" Maintainer:   Randy Stauner <randy@magnificent-tears.com>
" Last Change:  2019-09-06
" Version:      0.2

if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1

setlocal comments=:##
setlocal commentstring=##%s

if exists("loaded_matchit")
  let b:match_ignorecase = 1
  let b:match_words = "<:>," .
  \ "<\@<=[ou]l\>[^>]*\%(>\|$\):<\@<=li\>:<\@<=/[ou]l>," .
  \ "<\@<=dl\>[^>]*\%(>\|$\):<\@<=d[td]\>:<\@<=/dl>," .
  \ "<\@<=\([^/][^ \t>]*\)[^>]*\%(>\|$\):<\@<=/\1>"
endif
