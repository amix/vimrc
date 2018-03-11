" Extends standard help syntax with highlighting of Scala code.
"
" Place code between !sc! and !/sc! delimiters. These will be hidden if Vim is
" built with conceal support.

unlet! b:current_syntax

syntax include @ScalaCode syntax/scala.vim

if has('conceal')
  syntax region rgnScala matchgroup=Ignore concealends start='!sc!' end='!/sc!' contains=@ScalaCode
else
  syntax region rgnScala matchgroup=Ignore start='!sc!' end='!/sc!' contains=@ScalaCode
endif
