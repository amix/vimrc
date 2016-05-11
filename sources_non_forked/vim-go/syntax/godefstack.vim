if exists("b:current_syntax")
    finish
endif

syn match godefStackComment             '^".*'
syn match godefLinePrefix               '^[>\s]\s' nextgroup=godefStackEntryNumber contains=godefStackCurrentPosition
syn match godefStackEntryNumber         '\d\+' nextgroup=godefStackFilename skipwhite
syn match godefStackCurrentPosition     '>' contained
syn match godefStackFilename            '[^|]\+' contained nextgroup=godefStackEntryLocation
syn region godefStackEntryLocation      oneline start='|' end='|' contained contains=godefStackEntryLocationNumber
syn match godefStackEntryLocationNumber '\d\+' contained display

let b:current_syntax = "godefstack"

hi def link godefStackComment           Comment
hi def link godefStackCurrentPosition   Special
hi def link godefStackFilename          Directory
hi def link godefStackEntryLocationNumber LineNr
