if dracula#should_abort('lua')
    finish
endif

hi! link luaFunc  DraculaCyan
hi! link luaTable DraculaFg

" tbastos/vim-lua {{{

hi! link luaBraces       DraculaFg
hi! link luaBuiltIn      Constant
hi! link luaDocTag       Keyword
hi! link luaErrHand      DraculaCyan
hi! link luaFuncArgName  DraculaOrangeItalic
hi! link luaFuncCall     Function
hi! link luaLocal        Keyword
hi! link luaSpecialTable Constant
hi! link luaSpecialValue DraculaCyan

" }}}

" vim: fdm=marker ts=2 sts=2 sw=2 fdl=0:
