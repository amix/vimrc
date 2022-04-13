if exists('b:current_syntax')
    finish
endif

syn match aleFixerComment /^.*$/
syn match aleFixerName /\(^\|, \)'[^']*'/
syn match aleFixerHelp /^See :help ale-fix-configuration/

hi def link aleFixerComment Comment
hi def link aleFixerName String
hi def link aleFixerHelp Statement

let b:current_syntax = 'ale-fix-suggest'
