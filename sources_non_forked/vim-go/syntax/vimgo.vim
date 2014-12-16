if exists("b:current_syntax")
    finish
endif

let b:current_syntax = "vimgo"

syn match   goInterface /^\S*/
syn region  goTitle start="\%1l" end=":"

hi def link goInterface Type
hi def link goTitle Label
