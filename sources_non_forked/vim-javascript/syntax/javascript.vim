" Vim syntax file
" Language:     JavaScript
" Maintainer:   vim-javascript community
" URL:          https://github.com/pangloss/vim-javascript

if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'javascript'
endif

if !exists('g:javascript_conceal')
  let g:javascript_conceal = 0
endif

"" Drop fold if it is set but VIM doesn't support it.
let b:javascript_fold='true'
if version < 600    " Don't support the old version
  unlet! b:javascript_fold
endif

"" dollar sign is permittd anywhere in an identifier
setlocal iskeyword+=$

syntax sync fromstart

syntax match   jsNoise           /\%(:\|,\|\;\|\.\)/

"" Program Keywords
syntax keyword jsStorageClass   const var let
syntax keyword jsOperator       delete instanceof typeof void new in
syntax match   jsOperator       /\(!\||\|&\|+\|-\|<\|>\|=\|%\|\/\|*\|\~\|\^\)/
syntax keyword jsBooleanTrue    true
syntax keyword jsBooleanFalse   false

"" JavaScript comments
syntax keyword jsCommentTodo    TODO FIXME XXX TBD contained
syntax region  jsLineComment    start=+\/\/+ end=+$+ keepend contains=jsCommentTodo,@Spell
syntax region  jsEnvComment     start="\%^#!" end="$" display
syntax region  jsLineComment    start=+^\s*\/\/+ skip=+\n\s*\/\/+ end=+$+ keepend contains=jsCommentTodo,@Spell fold
syntax region  jsCvsTag         start="\$\cid:" end="\$" oneline contained
syntax region  jsComment        start="/\*"  end="\*/" contains=jsCommentTodo,jsCvsTag,@Spell fold

"" JSDoc / JSDoc Toolkit
if !exists("javascript_ignore_javaScriptdoc")
  syntax case ignore

  "" syntax coloring for javadoc comments (HTML)
  "syntax include @javaHtml <sfile>:p:h/html.vim
  "unlet b:current_syntax

  syntax region jsDocComment      matchgroup=jsComment start="/\*\*\s*"  end="\*/" contains=jsDocTags,jsCommentTodo,jsCvsTag,@jsHtml,@Spell fold

  " tags containing a param
  syntax match  jsDocTags         contained "@\(alias\|augments\|borrows\|class\|constructs\|default\|defaultvalue\|emits\|exception\|exports\|extends\|file\|fires\|kind\|listens\|member\|member[oO]f\|mixes\|module\|name\|namespace\|requires\|throws\|var\|variation\|version\)\>" nextgroup=jsDocParam skipwhite
  " tags containing type and param
  syntax match  jsDocTags         contained "@\(arg\|argument\|param\|property\)\>" nextgroup=jsDocType skipwhite
  " tags containing type but no param
  syntax match  jsDocTags         contained "@\(callback\|enum\|external\|this\|type\|typedef\|return\|returns\)\>" nextgroup=jsDocTypeNoParam skipwhite
  " tags containing references
  syntax match  jsDocTags         contained "@\(lends\|see\|tutorial\)\>" nextgroup=jsDocSeeTag skipwhite
  " other tags (no extra syntax)
  syntax match  jsDocTags         contained "@\(abstract\|access\|author\|classdesc\|constant\|const\|constructor\|copyright\|deprecated\|desc\|description\|event\|example\|file[oO]verview\|function\|global\|ignore\|inner\|instance\|license\|method\|mixin\|overview\|private\|protected\|public\|readonly\|since\|static\|todo\|summary\|undocumented\|virtual\)\>"

  syntax region jsDocType         start="{" end="}" oneline contained nextgroup=jsDocParam skipwhite
  syntax match  jsDocType         contained "\%(#\|\"\|\w\|\.\|:\|\/\)\+" nextgroup=jsDocParam skipwhite
  syntax region jsDocTypeNoParam  start="{" end="}" oneline contained
  syntax match  jsDocTypeNoParam  contained "\%(#\|\"\|\w\|\.\|:\|\/\)\+"
  syntax match  jsDocParam        contained "\%(#\|\"\|{\|}\|\w\|\.\|:\|\/\)\+"
  syntax region jsDocSeeTag       contained matchgroup=jsDocSeeTag start="{" end="}" contains=jsDocTags

  syntax case match
endif   "" JSDoc end

syntax case match

"" Syntax in the JavaScript code
syntax match   jsFuncCall        /\k\+\%(\s*(\)\@=/
syntax match   jsSpecial         "\v\\%(0|\\x\x\{2\}\|\\u\x\{4\}\|\c[A-Z]|.)" contained
syntax match   jsTemplateVar     "\${.\{-}}" contained
syntax region  jsStringD         start=+"+  skip=+\\\\\|\\$"+  end=+"+  contains=jsSpecial,@htmlPreproc,@Spell
syntax region  jsStringS         start=+'+  skip=+\\\\\|\\$'+  end=+'+  contains=jsSpecial,@htmlPreproc,@Spell
syntax region  jsTemplateString  start=+`+  skip=+\\\\\|\\$`+  end=+`+  contains=jsTemplateVar,jsSpecial,@htmlPreproc
syntax region  jsRegexpCharClass start=+\[+ skip=+\\.+ end=+\]+ contained
syntax match   jsRegexpBoundary   "\v%(\<@![\^$]|\\[bB])" contained
syntax match   jsRegexpBackRef   "\v\\[1-9][0-9]*" contained
syntax match   jsRegexpQuantifier "\v\\@<!%([?*+]|\{\d+%(,|,\d+)?})\??" contained
syntax match   jsRegexpOr        "\v\<@!\|" contained
syntax match   jsRegexpMod       "\v\(@<=\?[:=!>]" contained
syntax cluster jsRegexpSpecial   contains=jsSpecial,jsRegexpBoundary,jsRegexpBackRef,jsRegexpQuantifier,jsRegexpOr,jsRegexpMod
syntax region  jsRegexpGroup     start="\\\@<!(" end="\\\@<!)" contained contains=jsRegexpCharClass,@jsRegexpSpecial keepend
syntax region  jsRegexpString    start=+\(\(\(return\|case\)\s\+\)\@<=\|\(\([)\]"']\|\d\|\w\)\s*\)\@<!\)/\(\*\|/\)\@!+ skip=+\\.\|\[\(\\.\|[^]]\)*\]+ end=+/[gimy]\{,4}+ contains=jsRegexpCharClass,jsRegexpGroup,@jsRegexpSpecial,@htmlPreproc oneline keepend
syntax match   jsNumber          /\<-\=\d\+L\=\>\|\<0[xX]\x\+\>/
syntax keyword jsNumber          Infinity
syntax match   jsFloat           /\<-\=\%(\d\+\.\d\+\|\d\+\.\|\.\d\+\)\%([eE][+-]\=\d\+\)\=\>/
syntax match   jsObjectKey       /\<[a-zA-Z_$][0-9a-zA-Z_$]*\(\s*:\)\@=/ contains=jsFunctionKey contained
syntax match   jsFunctionKey     /\<[a-zA-Z_$][0-9a-zA-Z_$]*\(\s*:\s*function\s*\)\@=/ contained

if g:javascript_conceal == 1
  syntax keyword jsNull           null conceal cchar=ø
  syntax keyword jsThis           this conceal cchar=@
  syntax keyword jsReturn         return conceal cchar=⇚
  syntax keyword jsUndefined      undefined conceal cchar=¿
  syntax keyword jsNan            NaN conceal cchar=ℕ
  syntax keyword jsPrototype      prototype conceal cchar=¶
else
  syntax keyword jsNull           null
  syntax keyword jsThis           this
  syntax keyword jsReturn         return
  syntax keyword jsUndefined      undefined
  syntax keyword jsNan            NaN
  syntax keyword jsPrototype      prototype
endif

"" Statement Keywords
syntax keyword jsStatement      break continue with
syntax keyword jsConditional    if else switch
syntax keyword jsRepeat         do while for
syntax keyword jsLabel          case default
syntax keyword jsKeyword        yield
syntax keyword jsException      try catch throw finally

syntax keyword jsGlobalObjects   Array Boolean Date Function Iterator Number Object RegExp String Proxy ParallelArray ArrayBuffer DataView Float32Array Float64Array Int16Array Int32Array Int8Array Uint16Array Uint32Array Uint8Array Uint8ClampedArray Intl JSON Math console document window
syntax match   jsGlobalObjects  /\%(Intl\.\)\@<=\(Collator\|DateTimeFormat\|NumberFormat\)/

syntax keyword jsExceptions     Error EvalError InternalError RangeError ReferenceError StopIteration SyntaxError TypeError URIError

syntax keyword jsBuiltins       decodeURI decodeURIComponent encodeURI encodeURIComponent eval isFinite isNaN parseFloat parseInt uneval

syntax keyword jsFutureKeys     abstract enum int short boolean export interface static byte extends long super char final native synchronized class float package throws goto private transient debugger implements protected volatile double import public

"" DOM/HTML/CSS specified things

" DOM2 Objects
syntax keyword jsGlobalObjects  DOMImplementation DocumentFragment Document Node NodeList NamedNodeMap CharacterData Attr Element Text Comment CDATASection DocumentType Notation Entity EntityReference ProcessingInstruction
syntax keyword jsExceptions     DOMException

" DOM2 CONSTANT
syntax keyword jsDomErrNo       INDEX_SIZE_ERR DOMSTRING_SIZE_ERR HIERARCHY_REQUEST_ERR WRONG_DOCUMENT_ERR INVALID_CHARACTER_ERR NO_DATA_ALLOWED_ERR NO_MODIFICATION_ALLOWED_ERR NOT_FOUND_ERR NOT_SUPPORTED_ERR INUSE_ATTRIBUTE_ERR INVALID_STATE_ERR SYNTAX_ERR INVALID_MODIFICATION_ERR NAMESPACE_ERR INVALID_ACCESS_ERR
syntax keyword jsDomNodeConsts  ELEMENT_NODE ATTRIBUTE_NODE TEXT_NODE CDATA_SECTION_NODE ENTITY_REFERENCE_NODE ENTITY_NODE PROCESSING_INSTRUCTION_NODE COMMENT_NODE DOCUMENT_NODE DOCUMENT_TYPE_NODE DOCUMENT_FRAGMENT_NODE NOTATION_NODE

" HTML events and internal variables
syntax case ignore
syntax keyword jsHtmlEvents     onblur onclick oncontextmenu ondblclick onfocus onkeydown onkeypress onkeyup onmousedown onmousemove onmouseout onmouseover onmouseup onresize
syntax case match

" Follow stuff should be highligh within a special context
" While it can't be handled with context depended with Regex based highlight
" So, turn it off by default
if exists("javascript_enable_domhtmlcss")

    " DOM2 things
    syntax match jsDomElemAttrs     contained /\%(nodeName\|nodeValue\|nodeType\|parentNode\|childNodes\|firstChild\|lastChild\|previousSibling\|nextSibling\|attributes\|ownerDocument\|namespaceURI\|prefix\|localName\|tagName\)\>/
    syntax match jsDomElemFuncs     contained /\%(insertBefore\|replaceChild\|removeChild\|appendChild\|hasChildNodes\|cloneNode\|normalize\|isSupported\|hasAttributes\|getAttribute\|setAttribute\|removeAttribute\|getAttributeNode\|setAttributeNode\|removeAttributeNode\|getElementsByTagName\|getAttributeNS\|setAttributeNS\|removeAttributeNS\|getAttributeNodeNS\|setAttributeNodeNS\|getElementsByTagNameNS\|hasAttribute\|hasAttributeNS\)\>/ nextgroup=jsParen skipwhite
    " HTML things
    syntax match jsHtmlElemAttrs    contained /\%(className\|clientHeight\|clientLeft\|clientTop\|clientWidth\|dir\|id\|innerHTML\|lang\|length\|offsetHeight\|offsetLeft\|offsetParent\|offsetTop\|offsetWidth\|scrollHeight\|scrollLeft\|scrollTop\|scrollWidth\|style\|tabIndex\|title\)\>/
    syntax match jsHtmlElemFuncs    contained /\%(blur\|click\|focus\|scrollIntoView\|addEventListener\|dispatchEvent\|removeEventListener\|item\)\>/ nextgroup=jsParen skipwhite

    " CSS Styles in JavaScript
    syntax keyword jsCssStyles      contained color font fontFamily fontSize fontSizeAdjust fontStretch fontStyle fontVariant fontWeight letterSpacing lineBreak lineHeight quotes rubyAlign rubyOverhang rubyPosition
    syntax keyword jsCssStyles      contained textAlign textAlignLast textAutospace textDecoration textIndent textJustify textJustifyTrim textKashidaSpace textOverflowW6 textShadow textTransform textUnderlinePosition
    syntax keyword jsCssStyles      contained unicodeBidi whiteSpace wordBreak wordSpacing wordWrap writingMode
    syntax keyword jsCssStyles      contained bottom height left position right top width zIndex
    syntax keyword jsCssStyles      contained border borderBottom borderLeft borderRight borderTop borderBottomColor borderLeftColor borderTopColor borderBottomStyle borderLeftStyle borderRightStyle borderTopStyle borderBottomWidth borderLeftWidth borderRightWidth borderTopWidth borderColor borderStyle borderWidth borderCollapse borderSpacing captionSide emptyCells tableLayout
    syntax keyword jsCssStyles      contained margin marginBottom marginLeft marginRight marginTop outline outlineColor outlineStyle outlineWidth padding paddingBottom paddingLeft paddingRight paddingTop
    syntax keyword jsCssStyles      contained listStyle listStyleImage listStylePosition listStyleType
    syntax keyword jsCssStyles      contained background backgroundAttachment backgroundColor backgroundImage gackgroundPosition backgroundPositionX backgroundPositionY backgroundRepeat
    syntax keyword jsCssStyles      contained clear clip clipBottom clipLeft clipRight clipTop content counterIncrement counterReset cssFloat cursor direction display filter layoutGrid layoutGridChar layoutGridLine layoutGridMode layoutGridType
    syntax keyword jsCssStyles      contained marks maxHeight maxWidth minHeight minWidth opacity MozOpacity overflow overflowX overflowY verticalAlign visibility zoom cssText
    syntax keyword jsCssStyles      contained scrollbar3dLightColor scrollbarArrowColor scrollbarBaseColor scrollbarDarkShadowColor scrollbarFaceColor scrollbarHighlightColor scrollbarShadowColor scrollbarTrackColor

    " Highlight ways
    syntax match jsDotNotation      "\." nextgroup=jsPrototype,jsDomElemAttrs,jsDomElemFuncs,jsHtmlElemAttrs,jsHtmlElemFuncs
    syntax match jsDotNotation      "\.style\." nextgroup=jsCssStyles

endif "DOM/HTML/CSS

"" end DOM/HTML/CSS specified things


"" Code blocks
syntax cluster jsExpression contains=jsComment,jsLineComment,jsDocComment,jsTemplateString,jsStringD,jsStringS,jsRegexpString,jsNumber,jsFloat,jsThis,jsOperator,jsBooleanTrue,jsBooleanFalse,jsNull,jsFunction,jsArrowFunction,jsGlobalObjects,jsExceptions,jsFutureKeys,jsDomErrNo,jsDomNodeConsts,jsHtmlEvents,jsDotNotation,jsBracket,jsParen,jsBlock,jsFuncCall,jsUndefined,jsNan,jsKeyword,jsStorageClass,jsPrototype,jsBuiltins,jsNoise
syntax cluster jsAll        contains=@jsExpression,jsLabel,jsConditional,jsRepeat,jsReturn,jsStatement,jsTernaryIf,jsException
syntax region  jsBracket    matchgroup=jsBrackets     start="\[" end="\]" contains=@jsAll,jsParensErrB,jsParensErrC,jsBracket,jsParen,jsBlock,@htmlPreproc fold
syntax region  jsParen      matchgroup=jsParens       start="("  end=")"  contains=@jsAll,jsParensErrA,jsParensErrC,jsParen,jsBracket,jsBlock,@htmlPreproc fold
syntax region  jsBlock      matchgroup=jsBraces       start="{"  end="}"  contains=@jsAll,jsParensErrA,jsParensErrB,jsParen,jsBracket,jsBlock,jsObjectKey,@htmlPreproc fold
syntax region  jsFuncBlock  matchgroup=jsFuncBraces   start="{"  end="}"  contains=@jsAll,jsParensErrA,jsParensErrB,jsParen,jsBracket,jsBlock,@htmlPreproc contained fold
syntax region  jsTernaryIf  matchgroup=jsTernaryIfOperator start=+?+  end=+:+  contains=@jsExpression,jsTernaryIf

"" catch errors caused by wrong parenthesis
syntax match   jsParensError    ")\|}\|\]"
syntax match   jsParensErrA     contained "\]"
syntax match   jsParensErrB     contained ")"
syntax match   jsParensErrC     contained "}"

if main_syntax == "javascript"
  syntax sync clear
  syntax sync ccomment jsComment minlines=200
  syntax sync match jsHighlight grouphere jsBlock /{/
endif

if g:javascript_conceal == 1
  syntax match   jsFunction       /\<function\>/ nextgroup=jsFuncName,jsFuncArgs skipwhite conceal cchar=ƒ
else
  syntax match   jsFunction       /\<function\>/ nextgroup=jsFuncName,jsFuncArgs skipwhite
endif

syntax match   jsFuncName       contained /\<[a-zA-Z_$][0-9a-zA-Z_$]*/ nextgroup=jsFuncArgs skipwhite
syntax region  jsFuncArgs       contained matchgroup=jsFuncParens start='(' end=')' contains=jsFuncArgCommas,jsFuncArgRest nextgroup=jsFuncBlock keepend skipwhite skipempty
syntax match   jsFuncArgCommas  contained ','
syntax match   jsFuncArgRest    contained /\%(\.\.\.[a-zA-Z_$][0-9a-zA-Z_$]*\))/
syntax keyword jsArgsObj        arguments contained containedin=jsFuncBlock

syntax match jsArrowFunction /=>/

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_javascript_syn_inits")
  if version < 508
    let did_javascript_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink jsFuncArgRest          Special
  HiLink jsComment              Comment
  HiLink jsLineComment          Comment
  HiLink jsEnvComment           PreProc
  HiLink jsDocComment           Comment
  HiLink jsCommentTodo          Todo
  HiLink jsCvsTag               Function
  HiLink jsDocTags              Special
  HiLink jsDocSeeTag            Function
  HiLink jsDocType              Type
  HiLink jsDocTypeNoParam       Type
  HiLink jsDocParam             Label
  HiLink jsStringS              String
  HiLink jsStringD              String
  HiLink jsTemplateString       String
  HiLink jsTernaryIfOperator    Conditional
  HiLink jsRegexpString         String
  HiLink jsRegexpBoundary       SpecialChar
  HiLink jsRegexpQuantifier     SpecialChar
  HiLink jsRegexpOr             Conditional
  HiLink jsRegexpMod            SpecialChar
  HiLink jsRegexpBackRef        SpecialChar
  HiLink jsRegexpGroup          jsRegexpString
  HiLink jsRegexpCharClass      Character
  HiLink jsCharacter            Character
  HiLink jsPrototype            Special
  HiLink jsConditional          Conditional
  HiLink jsBranch               Conditional
  HiLink jsLabel                Label
  HiLink jsReturn               Statement
  HiLink jsRepeat               Repeat
  HiLink jsStatement            Statement
  HiLink jsException            Exception
  HiLink jsKeyword              Keyword
  HiLink jsArrowFunction        Type
  HiLink jsFunction             Type
  HiLink jsFuncName             Function
  HiLink jsArgsObj              Special
  HiLink jsError                Error
  HiLink jsParensError          Error
  HiLink jsParensErrA           Error
  HiLink jsParensErrB           Error
  HiLink jsParensErrC           Error
  HiLink jsOperator             Operator
  HiLink jsStorageClass         StorageClass
  HiLink jsThis                 Special
  HiLink jsNan                  Number
  HiLink jsNull                 Type
  HiLink jsUndefined            Type
  HiLink jsNumber               Number
  HiLink jsFloat                Float
  HiLink jsBooleanTrue          Boolean
  HiLink jsBooleanFalse         Boolean
  HiLink jsNoise                Noise
  HiLink jsBrackets             Noise
  HiLink jsParens               Noise
  HiLink jsBraces               Noise
  HiLink jsFuncBraces           Noise
  HiLink jsFuncParens           Noise
  HiLink jsSpecial              Special
  HiLink jsTemplateVar          Special
  HiLink jsGlobalObjects        Special
  HiLink jsExceptions           Special
  HiLink jsFutureKeys           Special
  HiLink jsBuiltins             Special

  HiLink jsDomErrNo             Constant
  HiLink jsDomNodeConsts        Constant
  HiLink jsDomElemAttrs         Label
  HiLink jsDomElemFuncs         PreProc

  HiLink jsHtmlEvents           Special
  HiLink jsHtmlElemAttrs        Label
  HiLink jsHtmlElemFuncs        PreProc

  HiLink jsCssStyles            Label

  delcommand HiLink
endif

" Define the htmlJavaScript for HTML syntax html.vim
"syntax clear htmlJavaScript
"syntax clear jsExpression
syntax cluster  htmlJavaScript       contains=@jsAll,jsBracket,jsParen,jsBlock
syntax cluster  javaScriptExpression contains=@jsAll,jsBracket,jsParen,jsBlock,@htmlPreproc
" Vim's default html.vim highlights all javascript as 'Special'
hi! def link javaScript              NONE

let b:current_syntax = "javascript"
if main_syntax == 'javascript'
  unlet main_syntax
endif
