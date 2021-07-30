if dracula#should_abort('yaml')
    finish
endif

hi! link yamlAlias           DraculaGreenItalicUnderline
hi! link yamlAnchor          DraculaPinkItalic
hi! link yamlBlockMappingKey DraculaCyan
hi! link yamlFlowCollection  DraculaPink
hi! link yamlFlowIndicator   Delimiter
hi! link yamlNodeTag         DraculaPink
hi! link yamlPlainScalar     DraculaYellow

