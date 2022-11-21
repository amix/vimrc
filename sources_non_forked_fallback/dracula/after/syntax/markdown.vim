if dracula#should_abort('markdown', 'mkd')
    finish
endif

if b:current_syntax ==# 'mkd'
" plasticboy/vim-markdown {{{1
  hi! link htmlBold       DraculaOrangeBold
  hi! link htmlBoldItalic DraculaOrangeBoldItalic
  hi! link htmlH1         DraculaPurpleBold
  hi! link htmlItalic     DraculaYellowItalic
  hi! link mkdBlockquote  DraculaYellowItalic
  hi! link mkdBold        DraculaOrangeBold
  hi! link mkdBoldItalic  DraculaOrangeBoldItalic
  hi! link mkdCode        DraculaGreen
  hi! link mkdCodeEnd     DraculaGreen
  hi! link mkdCodeStart   DraculaGreen
  hi! link mkdHeading     DraculaPurpleBold
  hi! link mkdInlineUrl   DraculaLink
  hi! link mkdItalic      DraculaYellowItalic
  hi! link mkdLink        DraculaPink
  hi! link mkdListItem    DraculaCyan
  hi! link mkdRule        DraculaComment
  hi! link mkdUrl         DraculaLink
"}}}1
elseif b:current_syntax ==# 'markdown'
" Builtin: {{{1
  hi! link markdownBlockquote        DraculaCyan
  hi! link markdownBold              DraculaOrangeBold
  hi! link markdownBoldItalic        DraculaOrangeBoldItalic
  hi! link markdownCodeBlock         DraculaGreen
  hi! link markdownCode              DraculaGreen
  hi! link markdownCodeDelimiter     DraculaGreen
  hi! link markdownH1                DraculaPurpleBold
  hi! link markdownH2                markdownH1
  hi! link markdownH3                markdownH1
  hi! link markdownH4                markdownH1
  hi! link markdownH5                markdownH1
  hi! link markdownH6                markdownH1
  hi! link markdownHeadingDelimiter  markdownH1
  hi! link markdownHeadingRule       markdownH1
  hi! link markdownItalic            DraculaYellowItalic
  hi! link markdownLinkText          DraculaPink
  hi! link markdownListMarker        DraculaCyan
  hi! link markdownOrderedListMarker DraculaCyan
  hi! link markdownRule              DraculaComment
  hi! link markdownUrl               DraculaLink
"}}}
endif

" vim: fdm=marker ts=2 sts=2 sw=2 fdl=0:
