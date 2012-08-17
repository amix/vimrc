if exists("b:current_syntax")
  finish
endif

runtime! syntax/css.vim
runtime! after/syntax/css.vim
" load files from vim-css3-syntax plugin (https://github.com/hail2u/vim-css3-syntax)
runtime! after/syntax/css/*.vim

syn case ignore

syn region lessDefinition transparent matchgroup=cssBraces start='{' end='}' contains=css.*Attr,css.*Prop,cssComment,cssValue.*,cssColor,cssTagName,cssPseudoClass,cssUrl,cssImportant,cssError,cssStringQ,cssStringQQ,cssFunction,cssUnicodeEscape,lessDefinition,lessComment,lessClassChar,lessVariable,lessMixinChar,lessAmpersandChar,lessFunction,@cssColors fold

syn match lessVariable "@[[:alnum:]_-]\+" contained 
syn match lessVariable "@[[:alnum:]_-]\+" nextgroup=lessVariableAssignment skipwhite
syn match lessVariableAssignment ":" contained nextgroup=lessVariableValue skipwhite
syn match lessVariableValue ".*;"me=e-1 contained contains=lessVariable,lessOperator,lessDefault,cssValue.*,@cssColors "me=e-1 means that the last char of the pattern is not highlighted

syn match lessOperator "+" contained
syn match lessOperator "-" contained
syn match lessOperator "/" contained
syn match lessOperator "*" contained

syn match lessDefault "!default" contained

syn match lessMixinChar "\.[[:alnum:]_-]\@=" contained nextgroup=lessClass
syn match lessAmpersandChar "&" contained nextgroup=lessClass,cssPseudoClass
syn match lessClass "[[:alnum:]_-]\+" contained

syn keyword lessFunction lighten darken saturate desaturate fadein fadeout spin hue saturation lightness containedin=cssDefinition contained 

syn match lessComment "//.*$" contains=@Spell

hi def link lessVariable Special
hi def link lessVariableValue Constant
hi def link lessDefault Special
hi def link lessComment Comment
hi def link lessFunction Function
hi def link lessMixinChar Special
hi def link lessAmpersandChar Special
hi def link lessClass PreProc

let b:current_syntax = "less"

