if exists("b:did_indent")
  finish
endif

runtime! indent/elixir.vim
unlet! b:did_indent
setlocal indentexpr=

let s:cpo_save = &cpo
set cpo&vim

if exists("b:eelixir_subtype")
  exe "runtime! indent/".b:eelixir_subtype.".vim"
else
  runtime! indent/html.vim
endif
unlet! b:did_indent

if &l:indentexpr == ''
  if &l:cindent
    let &l:indentexpr = 'cindent(v:lnum)'
  else
    let &l:indentexpr = 'indent(prevnonblank(v:lnum-1))'
  endif
endif
let b:eelixir_subtype_indentexpr = &l:indentexpr

let b:did_indent = 1

setlocal indentexpr=GetEelixirIndent()
setlocal indentkeys=o,O,*<Return>,<>>,{,},0),0],o,O,!^F,=end,=else,=elsif,=catch,=after,=rescue

" Only define the function once.
if exists("*GetEelixirIndent")
  finish
endif

function! GetEelixirIndent(...)
  if a:0 && a:1 == '.'
    let v:lnum = line('.')
  elseif a:0 && a:1 =~ '^\d'
    let v:lnum = a:1
  endif
  let vcol = col('.')
  call cursor(v:lnum,1)
  let inelixir = searchpair('<%','','%>','W')
  call cursor(v:lnum,vcol)
  if inelixir && getline(v:lnum) !~ '^<%\|^\s*%>'
    let ind = GetElixirIndent()
  else
    exe "let ind = ".b:eelixir_subtype_indentexpr
  endif
  let lnum = prevnonblank(v:lnum-1)
  let line = getline(lnum)
  let cline = getline(v:lnum)
  if cline =~# '^\s*<%\s*\%(end\|else\|elsif\|catch\|after\|rescue\)\>.*%>'
    let ind -= &sw
  elseif line =~# '\S\s*<%\s*end\s*%>'
    let ind -= &sw
  endif
  if line =~# '<%[=%]\=\s*.*\(\<do\|->\)\s*%>' ||
        \ line =~# '<%\s*\%(else\|elsif\|catch\|after\|rescue\)\>.*%>'
    let ind += &sw
  endif
  if cline =~# '^\s*%>\s*$'
    let ind -= &sw
  endif
  return ind
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save
