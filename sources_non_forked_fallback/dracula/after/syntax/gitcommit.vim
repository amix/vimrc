if dracula#should_abort('gitcommit')
    finish
endif

" The following two are misnomers. Colors are correct.
hi! link diffFile    DraculaGreen
hi! link diffNewFile DraculaRed

hi! link diffAdded   DraculaGreen
hi! link diffLine    DraculaCyanItalic
hi! link diffRemoved DraculaRed

