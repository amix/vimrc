if dracula#should_abort('ruby')
    finish
endif

if ! exists('g:ruby_operators')
    let g:ruby_operators=1
endif

hi! link rubyBlockArgument          DraculaOrangeItalic
hi! link rubyBlockParameter         DraculaOrangeItalic
hi! link rubyCurlyBlock             DraculaPink
hi! link rubyGlobalVariable         DraculaPurple
hi! link rubyInstanceVariable       DraculaPurpleItalic
hi! link rubyInterpolationDelimiter DraculaPink
hi! link rubyRegexpDelimiter        DraculaRed
hi! link rubyStringDelimiter        DraculaYellow
