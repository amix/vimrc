if dracula#should_abort('javascript', 'javascriptreact', 'javascript.jsx')
  finish
endif

hi! link javaScriptBraces   Delimiter
hi! link javaScriptNumber   Constant
hi! link javaScriptNull     Constant
hi! link javaScriptFunction Keyword

" pangloss/vim-javascript {{{

hi! link jsArrowFunction           Operator
hi! link jsBuiltins                DraculaCyan
hi! link jsClassDefinition         DraculaCyan
hi! link jsClassMethodType         Keyword
hi! link jsDestructuringAssignment DraculaOrangeItalic
hi! link jsDocParam                DraculaOrangeItalic
hi! link jsDocTags                 Keyword
hi! link jsDocType                 Type
hi! link jsDocTypeBrackets         DraculaCyan
hi! link jsFuncArgOperator         Operator
hi! link jsFuncArgs                DraculaOrangeItalic
hi! link jsFunction                Keyword
hi! link jsNull                    Constant
hi! link jsObjectColon             DraculaPink
hi! link jsSuper                   DraculaPurpleItalic
hi! link jsTemplateBraces          Special
hi! link jsThis                    DraculaPurpleItalic
hi! link jsUndefined               Constant

"}}}

" maxmellon/vim-jsx-pretty {{{

hi! link jsxTag             Keyword
hi! link jsxTagName         Keyword
hi! link jsxComponentName   Type
hi! link jsxCloseTag        Type
hi! link jsxAttrib          DraculaGreenItalic
hi! link jsxCloseString     Identifier
hi! link jsxOpenPunct       Identifier

" }}}

" vim: fdm=marker ts=2 sts=2 sw=2 fdl=0:
