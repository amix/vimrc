if exists("b:current_syntax")
  finish
endif

runtime! syntax/css.vim
runtime! after/syntax/css.vim
" load files from vim-css3-syntax plugin (https://github.com/hail2u/vim-css3-syntax)
runtime! after/syntax/css/*.vim

syn case ignore

syn region lessDefinition transparent matchgroup=cssBraces start='{' end='}' contains=css.*Attr,css.*Prop,cssComment,cssValue.*,cssColor,cssTagName,cssPseudoClass,cssUrl,cssImportant,cssError,cssStringQ,cssStringQQ,cssFunction,cssUnicodeEscape,lessDefinition,lessComment,lessClassChar,lessVariable,lessMixinChar,lessAmpersandChar,lessFunction,lessNestedSelector,@cssColors fold

syn match lessVariable "@[[:alnum:]_-]\+" contained
syn match lessVariable "@[[:alnum:]_-]\+" nextgroup=lessVariableAssignment skipwhite
syn match lessVariableAssignment ":" contained nextgroup=lessVariableValue skipwhite
syn match lessVariableValue ".*;"me=e-1 contained contains=lessVariable,lessOperator,lessDefault,cssValue.*,@cssColors "me=e-1 means that the last char of the pattern is not highlighted

syn match lessOperator "+" contained
syn match lessOperator "-" contained
syn match lessOperator "/" contained
syn match lessOperator "*" contained

syn match lessNestedSelector "[^/]* {"me=e-1 contained contains=cssTagName,cssAttributeSelector,lessAmpersandChar,lessVariable,lessMixinChar,lessFunction,lessNestedProperty
syn match lessNestedProperty "[[:alnum:]]\+:"me=e-1 contained

syn match lessDefault "!default" contained

syn match lessMixinChar "\.[[:alnum:]_-]\@=" contained nextgroup=lessClass
syn match lessAmpersandChar "&" contained nextgroup=lessClass,cssPseudoClass
syn match lessClass "[[:alnum:]_-]\+" contained

" functions {{{

" string functions
syn keyword lessFunction escape e % containedin=cssDefinition contained
" misc functions
syn keyword lessFunction color unit containedin=cssDefinition contained
" math functions
syn keyword lessFunction ceil floor percentage round containedin=cssDefinition contained
" color definition
syn keyword lessFunction rgb rgba argb hsl hsla hsv hsva containedin=cssDefinition contained
" color channel information
syn keyword lessFunction hue saturation lightness red green blue alpha luma containedin=cssDefinition contained
" color operations
syn keyword lessFunction saturate desaturate lighten darken fadein fadeout fade spin mix greyscale contrast containedin=cssDefinition contained
" color blending
syn keyword lessFunction multiply screen overlay softlight hardlight difference exclusion average negation containedin=cssDefinition contained

" }}}

syn match lessComment "//.*$" contains=@Spell

hi def link lessVariable Special
hi def link lessVariableValue Constant
hi def link lessDefault Special
hi def link lessComment Comment
hi def link lessFunction Function
hi def link lessMixinChar Special
hi def link lessAmpersandChar Special
hi def link lessNestedProperty Type
hi def link lessClass PreProc

let b:current_syntax = "less"
