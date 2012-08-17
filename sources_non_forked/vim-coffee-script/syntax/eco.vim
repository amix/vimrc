" Vim syntax file
" Language:		eco
" Maintainer:		Jay Adkisson
" Mostly stolen from eruby.vim

if !exists("g:eco_default_subtype")
  let g:eco_default_subtype = "html"
endif

if !exists("b:eco_subtype")
  let s:lines = getline(1)."\n".getline(2)."\n".getline(3)."\n".getline(4)."\n".getline(5)."\n".getline("$")
  let b:eco_subtype = matchstr(s:lines,'eco_subtype=\zs\w\+')
  if b:eco_subtype == ''
    let b:eco_subtype = matchstr(substitute(expand("%:t"),'\c\%(\.eco\)\+$','',''),'\.\zs\w\+$')
  endif
  if b:eco_subtype == 'rhtml'
    let b:eco_subtype = 'html'
  elseif b:eco_subtype == 'jst'
    let b:eco_subtype = 'html'
  elseif b:eco_subtype == 'rb'
    let b:eco_subtype = 'ruby'
  elseif b:eco_subtype == 'yml'
    let b:eco_subtype = 'yaml'
  elseif b:eco_subtype == 'js' || b:eco_subtype == 'json'
    let b:eco_subtype = 'javascript'
  elseif b:eco_subtype == 'txt'
    " Conventional; not a real file type
    let b:eco_subtype = 'text'
  elseif b:eco_subtype == ''
    if exists('b:current_syntax') && b:current_syntax != ''
      let b:eco_subtype = b:current_syntax
    else
      let b:eco_subtype = g:eco_default_subtype
    endif
  endif
endif

if exists("b:eco_subtype") && b:eco_subtype != '' && b:eco_subtype != 'eco'
  exec "runtime! syntax/".b:eco_subtype.".vim"
  syn include @coffeeTop syntax/coffee.vim
endif

syn cluster ecoRegions contains=ecoBlock,ecoExpression,ecoComment

syn region ecoBlock      matchgroup=ecoDelimiter start=/<%/      end=/%>/ contains=@coffeeTop containedin=ALLBUT,@ecoRegions keepend
syn region ecoExpression matchgroup=ecoDelimiter start=/<%[=\-]/ end=/%>/ contains=@coffeeTop containedin=ALLBUT,@ecoRegions keepend
syn region ecoComment    matchgroup=ecoComment   start=/<%#/     end=/%>/ contains=@coffeeTodo,@Spell containedin=ALLBUT,@ecoRegions keepend

" eco features not in coffeescript proper
syn keyword ecoEnd end containedin=@ecoRegions
syn match ecoIndentColon /\s+\w+:/ containedin=@ecoRegions

" Define the default highlighting.

hi def link ecoDelimiter    Delimiter
hi def link ecoComment      Comment
hi def link ecoEnd          coffeeConditional
hi def link ecoIndentColon  None

let b:current_syntax = 'eco'

" vim: nowrap sw=2 sts=2 ts=8:
