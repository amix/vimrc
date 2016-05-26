if (exists("b:did_ftplugin"))
  finish
endif
let b:did_ftplugin = 1

" Matchit support
if exists("loaded_matchit") && !exists("b:match_words")
  let b:match_ignorecase = 0

  let b:match_words = '\:\@<!\<\%(do\|fn\)\:\@!\>' .
        \ ':' .
        \ '\<\%(else\|elsif\|catch\|after\|rescue\)\:\@!\>' .
        \ ':' .
        \ '\:\@<!\<end\>' .
        \ ',{:},\[:\],(:)'
endif

setlocal comments=:#
setlocal commentstring=#\ %s

function! GetElixirFilename(word)
  let word = a:word

  " get first thing that starts uppercase, until the first space or end of line
  let word = substitute(word,'^\s*\(\u[^ ]\+\).*$','\1','g')

  " remove any trailing characters that don't look like a nested module
  let word = substitute(word,'\.\U.*$','','g')

  " replace module dots with slash
  let word = substitute(word,'\.','/','g')

  " remove any special chars
  let word = substitute(word,'[^A-z0-9-_/]','','g')

  " convert to snake_case
  let word = substitute(word,'\(\u\+\)\(\u\l\)','\1_\2','g')
  let word = substitute(word,'\(\u\+\)\(\u\l\)','\1_\2','g')
  let word = substitute(word,'\(\l\|\d\)\(\u\)','\1_\2','g')
  let word = substitute(word,'-','_','g')
  let word = tolower(word)

  return word
endfunction

let &l:path =
      \ join([
      \   getcwd().'/lib',
      \   getcwd().'/src',
      \   getcwd().'/deps/**/lib',
      \   getcwd().'/deps/**/src',
      \   &g:path
      \ ], ',')
setlocal includeexpr=GetElixirFilename(v:fname)
setlocal suffixesadd=.ex,.exs,.eex,.erl,.yrl,.hrl

setlocal formatoptions-=t formatoptions+=croqlj
