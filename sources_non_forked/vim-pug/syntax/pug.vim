" Vim syntax file
" Language: Pug
" Maintainer: Joshua Borton
" Credits: Tim Pope
" Filenames: *.pug

if exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'pug'
endif

silent! syntax include @htmlCoffeescript syntax/coffee.vim
unlet! b:current_syntax
silent! syntax include @htmlStylus syntax/stylus.vim
unlet! b:current_syntax
silent! syntax include @htmlCss syntax/css.vim
unlet! b:current_syntax
silent! syntax include @htmlMarkdown syntax/markdown.vim
unlet! b:current_syntax

syn case match

syn region  javascriptParenthesisBlock start="(" end=")" contains=@htmlJavascript contained keepend
syn cluster htmlJavascript add=javascriptParenthesisBlock

syn region  pugJavascript matchgroup=pugJavascriptOutputChar start="[!&]\==\|\~" skip=",\s*$" end="$" contained contains=@htmlJavascript keepend
syn region  pugJavascript matchgroup=pugJavascriptChar start="-" skip=",\s*$" end="$" contained contains=@htmlJavascript keepend
syn cluster pugTop contains=pugBegin,pugComment,pugHtmlComment,pugJavascript
syn match   pugBegin "^\s*\%([<>]\|&[^=~ ]\)\@!" nextgroup=pugTag,pugClassChar,pugIdChar,pugPlainChar,pugJavascript,pugScriptConditional,pugScriptStatement,pugPipedText
syn match   pugTag "+\?\w\+\%(:\w\+\)\=" contained contains=htmlTagName,htmlSpecialTagName nextgroup=@pugComponent
syn cluster pugComponent contains=pugAttributes,pugIdChar,pugBlockExpansionChar,pugClassChar,pugPlainChar,pugJavascript,pugTagBlockChar,pugTagInlineText
syntax keyword pugCommentTodo  contained TODO FIXME XXX TBD
syn match   pugComment '\(\s\+\|^\)\/\/.*$' contains=pugCommentTodo
syn region  pugCommentBlock start="\z(\s\+\|^\)\/\/.*$" end="^\%(\z1\s\|\s*$\)\@!" contains=pugCommentTodo keepend
syn region  pugHtmlConditionalComment start="<!--\%(.*\)>" end="<!\%(.*\)-->" contains=pugCommentTodo
syn region  pugAttributes matchgroup=pugAttributesDelimiter start="(" end=")" contained contains=@htmlJavascript,pugHtmlArg,htmlArg,htmlEvent,htmlCssDefinition nextgroup=@pugComponent
syn match   pugClassChar "\." contained nextgroup=pugClass
syn match   pugBlockExpansionChar ":\s\+" contained nextgroup=pugTag,pugClassChar,pugIdChar
syn match   pugIdChar "#[[{]\@!" contained nextgroup=pugId
syn match   pugClass "\%(\w\|-\)\+" contained nextgroup=@pugComponent
syn match   pugId "\%(\w\|-\)\+" contained nextgroup=@pugComponent
syn region  pugDocType start="^\s*\(!!!\|doctype\)" end="$"
" Unless I'm mistaken, syntax/html.vim requires
" that the = sign be present for these matches.
" This adds the matches back for pug.
syn keyword pugHtmlArg contained href title

syn match   pugPlainChar "\\" contained
syn region  pugInterpolation matchgroup=pugInterpolationDelimiter start="[#!]{" end="}" contains=@htmlJavascript
syn match   pugInterpolationEscape "\\\@<!\%(\\\\\)*\\\%(\\\ze#{\|#\ze{\)"
syn match   pugTagInlineText "\s.*$" contained contains=pugInterpolation,pugTextInlinePug
syn region  pugPipedText matchgroup=pugPipeChar start="|" end="$" contained contains=pugInterpolation,pugTextInlinePug nextgroup=pugPipedText skipnl
syn match   pugTagBlockChar "\.$" contained nextgroup=pugTagBlockText,pugTagBlockEnd skipnl
syn region  pugTagBlockText start="\%(\s*\)\S" end="\ze\n" contained contains=pugInterpolation,pugTextInlinePug nextgroup=pugTagBlockText,pugTagBlockEnd skipnl
syn region  pugTagBlockEnd start="\s*\S" end="$" contained contains=pugInterpolation,pugTextInlinePug nextgroup=pugBegin skipnl
syn region  pugTextInlinePug matchgroup=pugInlineDelimiter start="#\[" end="]" contains=pugTag keepend

syn region  pugJavascriptFilter matchgroup=pugFilter start="^\z(\s*\):javascript\s*$" end="^\%(\z1\s\|\s*$\)\@!" contains=@htmlJavascript
syn region  pugMarkdownFilter matchgroup=pugFilter start=/^\z(\s*\):\%(markdown\|marked\)\s*$/ end=/^\%(\z1\s\|\s*$\)\@!/ contains=@htmlMarkdown
syn region  pugStylusFilter matchgroup=pugFilter start="^\z(\s*\):stylus\s*$" end="^\%(\z1\s\|\s*$\)\@!" contains=@htmlStylus
syn region  pugPlainFilter matchgroup=pugFilter start="^\z(\s*\):\%(sass\|less\|cdata\)\s*$" end="^\%(\z1\s\|\s*$\)\@!"

syn match  pugScriptConditional "^\s*\<\%(if\|else\|else if\|elif\|unless\|while\|until\|case\|when\|default\)\>[?!]\@!"
syn match  pugScriptStatement "^\s*\<\%(each\|for\|block\|prepend\|append\|mixin\|extends\|include\)\>[?!]\@!"
syn region  pugScriptLoopRegion start="^\s*\(for \)" end="$" contains=pugScriptLoopKeywords
syn keyword  pugScriptLoopKeywords for in contained

syn region  pugJavascript start="^\z(\s*\)script\%(:\w\+\)\=" end="^\%(\z1\s\|\s*$\)\@!" contains=@htmlJavascript,pugJavascriptTag,pugCoffeescriptFilter keepend 

syn region  pugCoffeescriptFilter matchgroup=pugFilter start="^\z(\s*\):coffee-\?script\s*$" end="^\%(\z1\s\|\s*$\)\@!" contains=@htmlCoffeescript contained
syn region  pugJavascriptTag contained start="^\z(\s*\)script\%(:\w\+\)\=" end="$" contains=pugBegin,pugTag
syn region  pugCssBlock        start="^\z(\s*\)style" nextgroup=@pugComponent,pugError  end="^\%(\z1\s\|\s*$\)\@!" contains=@htmlCss keepend

syn match  pugError "\$" contained

hi def link pugPlainChar              Special
hi def link pugScriptConditional      PreProc
hi def link pugScriptLoopKeywords     PreProc
hi def link pugScriptStatement        PreProc
hi def link pugHtmlArg                htmlArg
hi def link pugAttributeString        String
hi def link pugAttributesDelimiter    Identifier
hi def link pugIdChar                 Special
hi def link pugClassChar              Special
hi def link pugBlockExpansionChar     Special
hi def link pugPipeChar               Special
hi def link pugTagBlockChar           Special
hi def link pugId                     Identifier
hi def link pugClass                  Type
hi def link pugInterpolationDelimiter Delimiter
hi def link pugInlineDelimiter        Delimiter
hi def link pugFilter                 PreProc
hi def link pugDocType                PreProc
hi def link pugCommentTodo            Todo
hi def link pugComment                Comment
hi def link pugCommentBlock           Comment
hi def link pugHtmlConditionalComment pugComment

let b:current_syntax = "pug"

if main_syntax == "pug"
  unlet main_syntax
endif
