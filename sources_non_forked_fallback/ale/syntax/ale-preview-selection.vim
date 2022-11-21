if exists('b:current_syntax')
    finish
endif

syn match alePreviewSelectionFilename /\v^([a-zA-Z]?:?[^:]+)/
syn match alPreviewNumber /\v:\d+:\d+$/

hi def link alePreviewSelectionFilename String
hi def link alePreviewNumber Number

let b:current_syntax = 'ale-preview-selection'
