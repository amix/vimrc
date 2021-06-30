if exists("b:current_syntax") || !exists("*FugitiveGitDir")
  finish
endif

call fugitive#BlameSyntax()

let b:current_syntax = "fugitiveblame"
