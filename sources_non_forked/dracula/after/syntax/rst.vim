if dracula#should_abort('rst')
    finish
endif

hi! link rstComment                             Comment
hi! link rstTransition                          Comment
hi! link rstCodeBlock                           DraculaGreen
hi! link rstInlineLiteral                       DraculaGreen
hi! link rstLiteralBlock                        DraculaGreen
hi! link rstQuotedLiteralBlock                  DraculaGreen
hi! link rstStandaloneHyperlink                 DraculaLink
hi! link rstStrongEmphasis                      DraculaOrangeBold
hi! link rstSections                            DraculaPurpleBold
hi! link rstEmphasis                            DraculaYellowItalic
hi! link rstDirective                           Keyword
hi! link rstSubstitutionDefinition              Keyword
hi! link rstCitation                            String
hi! link rstExDirective                         String
hi! link rstFootnote                            String
hi! link rstCitationReference                   Tag
hi! link rstFootnoteReference                   Tag
hi! link rstHyperLinkReference                  Tag
hi! link rstHyperlinkTarget                     Tag
hi! link rstInlineInternalTargets               Tag
hi! link rstInterpretedTextOrHyperlinkReference Tag
hi! link rstTodo                                Todo
