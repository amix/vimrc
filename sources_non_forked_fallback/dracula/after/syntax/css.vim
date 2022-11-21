if dracula#should_abort('css')
    finish
endif

hi! link cssAttrComma         Delimiter
hi! link cssAttrRegion        DraculaPink
hi! link cssAttributeSelector DraculaGreenItalic
hi! link cssBraces            Delimiter
hi! link cssFunctionComma     Delimiter
hi! link cssNoise             DraculaPink
hi! link cssProp              DraculaCyan
hi! link cssPseudoClass       DraculaPink
hi! link cssPseudoClassId     DraculaGreenItalic
hi! link cssUnitDecorators    DraculaPink
hi! link cssVendor            DraculaGreenItalic
