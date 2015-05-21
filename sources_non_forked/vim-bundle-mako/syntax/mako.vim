" Vim syntax file
" Language:     Mako
" Maintainer:   Armin Ronacher <armin.ronacher@active-4.com>
" URL:          http://lucumr.pocoo.org/
" Last Change:  2013-05-01
" Version:      0.6.1+
"
" Thanks to Brine Rue <brian@lolapps.com> who noticed a bug in the
" delimiter handling.
"
" Known Limitations
"   the <%text> block does not have correct attributes

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = "html"
endif

"Source the html syntax file
ru! syntax/html.vim
unlet b:current_syntax

" tell html.vim what syntax groups should take precedence (see :help html.vim)
syn cluster htmlPreproc add=makoLine,makoVariable,makoTag,makoDocComment,makoDefEnd,makoText,makoDelim,makoEnd,makoComment,makoEscape

"Put the python syntax file in @pythonTop
syn include @pythonTop syntax/python.vim

" End keywords
syn keyword makoEnd contained endfor endwhile endif endtry enddef

" Block rules
syn region makoLine matchgroup=makoDelim start=#^\s*%# end=#$# keepend contains=@pythonTop,makoEnd
syn region makoBlock matchgroup=makoDelim start=#<%!\?# end=#%># keepend contains=@pythonTop,makoEnd

" Variables
syn region makoNested start="{" end="}" transparent display contained contains=makoNested,@pythonTop
syn region makoVariable matchgroup=makoDelim start=#\${# end=#}# contains=makoNested,@pythonTop

" Comments
syn region makoComment start="^\s*##" end="$"
syn region makoDocComment matchgroup=makoDelim start="<%doc>" end="</%doc>" keepend

" Literal Blocks
syn region makoText matchgroup=makoDelim start="<%text[^>]*>" end="</%text>"

" Attribute Sublexing
syn match makoAttributeKey containedin=makoTag contained "[a-zA-Z_][a-zA-Z0-9_]*="
syn region makoAttributeValue containedin=makoTag contained start=/"/ skip=/\\"/ end=/"/
syn region makoAttributeValue containedin=MakoTag contained start=/'/ skip=/\\'/ end=/'/

" Tags
syn region makoTag matchgroup=makoDelim start="<%\(def\|call\|page\|include\|namespace\|inherit\|block\|[a-zA-Z_][a-zA-Z0-9_]*:[a-zA-Z_][a-zA-Z0-9_]*\)\>" end="/\?>"
syn match makoDelim "</%\(def\|call\|namespace\|block\|[a-zA-Z_][a-zA-Z0-9_]*:[a-zA-Z_][a-zA-Z0-9_]*\)>"

syn region  makoJavaScript matchgroup=makoDelim start=+<%block .*js.*>+ keepend end=+</%block>+ contains=@htmlJavaScript,htmlCssStyleComment,htmlScriptTag,@htmlPreproc,makoLine,makoBlock,makoVariable
syn region makoCssStyle matchgroup=makoDelim start=+<%block .*css.*>+ keepend end=+</%block>+ contains=@htmlCss,htmlTag,htmlEndTag,htmlCssStyleComment,@htmlPreproc,makoLine,makoBlock,makoVariable

" Newline Escapes
syn match makoEscape /\\$/

" Default highlighting links
if version >= 508 || !exists("did_mako_syn_inits")
  if version < 508
    let did_mako_syn_inits = 1
    com -nargs=+ HiLink hi link <args>
  else
    com -nargs=+ HiLink hi def link <args>
  endif

  HiLink makoDocComment makoComment
  HiLink makoDefEnd makoDelim

  HiLink makoAttributeKey Type
  HiLink makoAttributeValue String
  HiLink makoText Normal
  HiLink makoDelim Preproc
  HiLink makoEnd Keyword
  HiLink makoComment Comment
  HiLink makoEscape Special

  delc HiLink
endif

let b:current_syntax = "html"
