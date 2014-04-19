" Vim syntax file
" Language: Jade
" Maintainer: Joshua Borton
" Credits: Tim Pope
" Filenames: *.jade

if exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'jade'
endif

runtime! syntax/html.vim
runtime! syntax/html/html5.vim
silent! syntax include @htmlCoffeescript syntax/coffee.vim
unlet! b:current_syntax
silent! syntax include @htmlStylus syntax/stylus.vim
unlet! b:current_syntax
silent! syntax include @htmlMarkdown syntax/markdown.vim
unlet! b:current_syntax

syn case match

syn region  javascriptParenthesisBlock start="(" end=")" contains=@htmlJavascript contained keepend
syn cluster htmlJavascript add=javascriptParenthesisBlock

syn region  jadeJavascript matchgroup=jadeJavascriptOutputChar start="[!&]\==\|\~" skip=",\s*$" end="$" contained contains=@htmlJavascript keepend
syn region  jadeJavascript matchgroup=jadeJavascriptChar start="-" skip=",\s*$" end="$" contained contains=@htmlJavascript keepend
syn cluster jadeTop contains=jadeBegin,jadeComment,jadeHtmlComment,jadeJavascript
syn match   jadeBegin "^\s*\%([<>]\|&[^=~ ]\)\@!" nextgroup=jadeTag,jadeClassChar,jadeIdChar,jadePlainChar,jadeJavascript,jadeScriptConditional,jadeScriptStatement
syn match   jadeTag "+\?\w\+\%(:\w\+\)\=" contained contains=htmlTagName,htmlSpecialTagName nextgroup=@jadeComponent
syn cluster jadeComponent contains=jadeAttributes,jadeIdChar,jadeBlockExpansionChar,jadeClassChar,jadePlainChar,jadeJavascript
syn match   jadeComment '\s*\/\/.*$'
syn region  jadeHtmlComment start="^\z(\s*\)/"  end="^\%(\z1\s\|\s*$\)\@!"
syn region  jadeAttributes matchgroup=jadeAttributesDelimiter start="(" end=")" contained contains=@htmlJavascript,jadeHtmlArg,htmlArg,htmlEvent,htmlCssDefinition nextgroup=@jadeComponent
syn match   jadeClassChar "\." contained nextgroup=jadeClass
syn match   jadeBlockExpansionChar ":\s" contained nextgroup=jadeTag
syn match   jadeIdChar "#{\@!" contained nextgroup=jadeId
syn match   jadeClass "\%(\w\|-\)\+" contained nextgroup=@jadeComponent
syn match   jadeId "\%(\w\|-\)\+" contained nextgroup=@jadeComponent
syn region  jadeDocType start="^\s*\(!!!\|doctype\)" end="$"
" Unless I'm mistaken, syntax/html.vim requires
" that the = sign be present for these matches.
" This adds the matches back for jade.
syn keyword jadeHtmlArg contained href title

syn match   jadePlainChar "\\" contained
syn region  jadeInterpolation matchgroup=jadeInterpolationDelimiter start="#{" end="}" contains=@htmlJavascript
syn match   jadeInterpolationEscape "\\\@<!\%(\\\\\)*\\\%(\\\ze#{\|#\ze{\)"

syn region  jadeJavascriptFilter matchgroup=jadeFilter start="^\z(\s*\):javascript\s*$" end="^\%(\z1\s\|\s*$\)\@!" contains=@htmlJavascript
syn region  jadeCoffeescriptFilter matchgroup=jadeFilter start="^\z(\s*\):coffeescript\s*$" end="^\%(\z1\s\|\s*$\)\@!" contains=@htmlCoffeescript
syn region  jadeMarkdownFilter matchgroup=jadeFilter start=/^\z(\s*\):markdown\s*$/ end=/^\%(\z1\s\|\s*$\)\@!/ contains=@htmlMarkdown
syn region  jadeStylusFilter matchgroup=jadeFilter start="^\z(\s*\):stylus\s*$" end="^\%(\z1\s\|\s*$\)\@!" contains=@htmlStylus
syn region  jadePlainFilter matchgroup=jadeFilter start="^\z(\s*\):\%(sass\|less\|cdata\)\s*$" end="^\%(\z1\s\|\s*$\)\@!"

syn match  jadeScriptConditional "\<\%(if\|else\|unless\|while\|for\|until\|case\|when\|default\)\>[?!]\@!"
syn match  jadeScriptStatement "\<\%(each\|block\|prepend\|append\|mixin\|extends\|include\)\>[?!]\@!"

syn region  jadeJavascript start="^\z(\s*\)script\%(:\w\+\)\=" end="^\%(\z1\s\|\s*$\)\@!" contains=@htmlJavascript,jadeJavascriptTag keepend 
syn region  jadeJavascriptTag contained start="^\z(\s*\)script\%(:\w\+\)\=" end="$" contains=jadeBegin,jadeTag
syn region  jadeCssBlock        start="^\z(\s*\)style" nextgroup=@jadeComponent,jadeError  end="^\%(\z1\s\|\s*$\)\@!" contains=@jadeTop,@htmlCss keepend

syn match  jadeError "\$" contained

hi def link jadePlainChar              Special
hi def link jadeScriptConditional      PreProc
hi def link jadeScriptStatement        PreProc
hi def link jadeHtmlArg                htmlArg
hi def link jadeAttributeString        String
hi def link jadeAttributesDelimiter    Identifier
hi def link jadeIdChar                 Special
hi def link jadeClassChar              Special
hi def link jadeBlockExpansionChar     Special
hi def link jadeId                     Identifier
hi def link jadeClass                  Type
hi def link jadeInterpolationDelimiter Delimiter
hi def link jadeFilter                 PreProc
hi def link jadeDocType                PreProc
hi def link jadeComment                Comment
hi def link jadeHtmlComment            jadeComment

let b:current_syntax = "jade"

if main_syntax == "jade"
  unlet main_syntax
endif
