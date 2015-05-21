" Language:    CoffeeScript
" Maintainer:  Mick Koch <mick@kochm.co>
" URL:         http://github.com/kchmck/vim-coffee-script
" License:     WTFPL

" Load the coffee and html indent functions.
silent! unlet b:did_indent
runtime indent/coffee.vim
let s:coffeeIndentExpr = &l:indentexpr

" Load html last so it can overwrite coffee settings.
silent! unlet b:did_indent
runtime indent/html.vim
let s:htmlIndentExpr = &l:indentexpr

" Inject our wrapper indent function.
setlocal indentexpr=GetCoffeeHtmlIndent(v:lnum)

function! GetCoffeeHtmlIndent(curlinenum)
  " See if we're inside a coffeescript block.
  let scriptlnum = searchpair('<script [^>]*type="text/coffeescript"[^>]*>', '',
  \                           '</script>', 'bWn')
  let prevlnum = prevnonblank(a:curlinenum)

  " If we're in the script block and the previous line isn't the script tag
  " itself, use coffee indenting.
  if scriptlnum && scriptlnum != prevlnum
    exec 'return ' s:coffeeIndentExpr
  endif

  " Otherwise use html indenting.
  exec 'return ' s:htmlIndentExpr
endfunction
