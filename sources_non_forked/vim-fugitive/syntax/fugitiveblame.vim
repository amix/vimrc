if exists("b:current_syntax")
  finish
endif

call fugitive#BlameSyntax()

let b:current_syntax = "fugitiveblame"
