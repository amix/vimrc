if exists('b:did_indent')
  finish
endif

runtime! indent/coffee.vim

let b:did_indent = 1

setlocal indentexpr=GetLitCoffeeIndent()

if exists('*GetLitCoffeeIndent')
  finish
endif

function GetLitCoffeeIndent()
  if searchpair('^    \|\t', '', '$', 'bWnm') > 0
    return GetCoffeeIndent(v:lnum)
  else
    return -1
  endif
endfunc

