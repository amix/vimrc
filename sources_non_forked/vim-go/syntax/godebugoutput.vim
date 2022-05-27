if exists("b:current_syntax")
  finish
endif

syn match godebugOutputErr '^ERR:.*'
syn match godebugOutputOut '^OUT:.*'

let b:current_syntax = "godebugoutput"

hi def link godebugOutputErr Comment
hi def link godebugOutputOut Normal

" vim: sw=2 ts=2 et
