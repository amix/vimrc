if exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'eelixir'
endif

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
  exe "runtime! syntax/".b:eelixir_subtype.".vim"
  unlet! b:current_syntax
endif

syn include @elixirTop syntax/elixir.vim

syn cluster eelixirRegions contains=eelixirBlock,eelixirExpression,eelixirComment

exe 'syn region  eelixirExpression matchgroup=eelixirDelimiter start="<%"  end="%\@<!%>" contains=@elixirTop  containedin=ALLBUT,@eelixirRegions keepend'
exe 'syn region  eelixirExpression matchgroup=eelixirDelimiter start="<%=" end="%\@<!%>" contains=@elixirTop  containedin=ALLBUT,@eelixirRegions keepend'
exe 'syn region  eelixirQuote      matchgroup=eelixirDelimiter start="<%%" end="%\@<!%>" contains=@elixirTop  containedin=ALLBUT,@eelixirRegions keepend'
exe 'syn region  eelixirComment    matchgroup=eelixirDelimiter start="<%#" end="%\@<!%>" contains=elixirTodo,@Spell containedin=ALLBUT,@eelixirRegions keepend'

" Define the default highlighting.

hi def link eelixirDelimiter PreProc
hi def link eelixirComment   Comment

let b:current_syntax = 'eelixir'

if main_syntax == 'eelixir'
  unlet main_syntax
endif
