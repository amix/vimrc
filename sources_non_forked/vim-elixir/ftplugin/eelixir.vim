if exists("b:did_ftplugin")
  finish
endif

let s:save_cpo = &cpo
set cpo-=C

let s:undo_ftplugin = ""
let s:browsefilter = "All Files (*.*)\t*.*\n"
let s:match_words = ""

if !exists("g:eelixir_default_subtype")
  let g:eelixir_default_subtype = "html"
endif

if !exists("b:eelixir_subtype")
  let s:lines = getline(1)."\n".getline(2)."\n".getline(3)."\n".getline(4)."\n".getline(5)."\n".getline("$")
  let b:eelixir_subtype = matchstr(s:lines,'eelixir_subtype=\zs\w\+')
  if b:eelixir_subtype == ''
    let b:eelixir_subtype = matchstr(&filetype,'^eex\.\zs\w\+')
  endif
  if b:eelixir_subtype == ''
    let b:eelixir_subtype = matchstr(substitute(expand("%:t"),'\c\%(\.eex\|\.eelixir\)\+$','',''),'\.\zs\w\+$')
  endif
  if b:eelixir_subtype == 'ex'
    let b:eelixir_subtype = 'elixir'
  elseif b:eelixir_subtype == 'exs'
    let b:eelixir_subtype = 'elixir'
  elseif b:eelixir_subtype == 'yml'
    let b:eelixir_subtype = 'yaml'
  elseif b:eelixir_subtype == 'js'
    let b:eelixir_subtype = 'javascript'
  elseif b:eelixir_subtype == 'txt'
    " Conventional; not a real file type
    let b:eelixir_subtype = 'text'
  elseif b:eelixir_subtype == ''
    let b:eelixir_subtype = g:eelixir_default_subtype
  endif
endif

if exists("b:eelixir_subtype") && b:eelixir_subtype != ''
  exe "runtime! ftplugin/".b:eelixir_subtype.".vim ftplugin/".b:eelixir_subtype."_*.vim ftplugin/".b:eelixir_subtype."/*.vim"
else
  runtime! ftplugin/html.vim ftplugin/html_*.vim ftplugin/html/*.vim
endif
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

runtime! ftplugin/elixir.vim ftplugin/elixir_*.vim ftplugin/elixir/*.vim
let b:did_ftplugin = 1

" Combine the new set of values with those previously included.
if exists("b:undo_ftplugin")
  let s:undo_ftplugin = b:undo_ftplugin . " | " . s:undo_ftplugin
endif
if exists ("b:browsefilter")
  let s:browsefilter = substitute(b:browsefilter,'\cAll Files (\*\.\*)\t\*\.\*\n','','') . s:browsefilter
endif
if exists("b:match_words")
  let s:match_words = b:match_words . ',' . s:match_words
endif

" Load the combined list of match_words for matchit.vim
if exists("loaded_matchit")
  let b:match_words = s:match_words
endif

setlocal comments=:<%#
setlocal commentstring=<%#\ %s\ %>

let b:undo_ftplugin = "setl cms< "
      \ " | unlet! b:browsefilter b:match_words | " . s:undo_ftplugin

let &cpo = s:save_cpo
