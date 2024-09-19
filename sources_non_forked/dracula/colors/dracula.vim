" Dracula Theme: {{{
"
" https://github.com/zenorocha/dracula-theme
"
" Copyright 2016, All rights reserved
"
" Code licensed under the MIT license
" http://zenorocha.mit-license.org
"
" @author Trevor Heins <@heinst>
" @author Éverton Ribeiro <nuxlli@gmail.com>
" @author Derek Sifford <dereksifford@gmail.com>
" @author Zeno Rocha <hi@zenorocha.com>
scriptencoding utf8
" }}}

" Configuration: {{{

if v:version > 580
  highlight clear
  if exists('syntax_on')
    syntax reset
  endif
endif

let g:colors_name = 'dracula'

if !(has('termguicolors') && &termguicolors) && !has('gui_running') && &t_Co != 256
  finish
endif

" Palette: {{{2

let s:fg        = g:dracula#palette.fg

let s:bglighter = g:dracula#palette.bglighter
let s:bglight   = g:dracula#palette.bglight
let s:bg        = g:dracula#palette.bg
let s:bgdark    = g:dracula#palette.bgdark
let s:bgdarker  = g:dracula#palette.bgdarker

let s:comment   = g:dracula#palette.comment
let s:selection = g:dracula#palette.selection
let s:subtle    = g:dracula#palette.subtle

let s:cyan      = g:dracula#palette.cyan
let s:green     = g:dracula#palette.green
let s:orange    = g:dracula#palette.orange
let s:pink      = g:dracula#palette.pink
let s:purple    = g:dracula#palette.purple
let s:red       = g:dracula#palette.red
let s:yellow    = g:dracula#palette.yellow

let s:none      = ['NONE', 'NONE']

if has('nvim')
  for s:i in range(16)
    let g:terminal_color_{s:i} = g:dracula#palette['color_' . s:i]
  endfor
endif

if has('terminal')
  let g:terminal_ansi_colors = []
  for s:i in range(16)
    call add(g:terminal_ansi_colors, g:dracula#palette['color_' . s:i])
  endfor
endif

" }}}2
" User Configuration: {{{2

if !exists('g:dracula_bold')
  let g:dracula_bold = 1
endif

if !exists('g:dracula_italic')
  let g:dracula_italic = 1
endif

if !exists('g:dracula_strikethrough')
  let g:dracula_strikethrough = 1
endif

if !exists('g:dracula_underline')
  let g:dracula_underline = 1
endif

if !exists('g:dracula_undercurl')
  let g:dracula_undercurl = g:dracula_underline
endif

if !exists('g:dracula_full_special_attrs_support')
  let g:dracula_full_special_attrs_support = has('gui_running')
endif

if !exists('g:dracula_inverse')
  let g:dracula_inverse = 1
endif

if !exists('g:dracula_colorterm')
  let g:dracula_colorterm = 1
endif

if !exists('g:dracula_high_contrast_diff')
  let g:dracula_high_contrast_diff = 0
endif

"}}}2
" Script Helpers: {{{2

let s:attrs = {
      \ 'bold': g:dracula_bold == 1 ? 'bold' : 0,
      \ 'italic': g:dracula_italic == 1 ? 'italic' : 0,
      \ 'strikethrough': g:dracula_strikethrough == 1 ? 'strikethrough' : 0,
      \ 'underline': g:dracula_underline == 1 ? 'underline' : 0,
      \ 'undercurl': g:dracula_undercurl == 1 ? 'undercurl' : 0,
      \ 'inverse': g:dracula_inverse == 1 ? 'inverse' : 0,
      \}

function! s:h(scope, fg, ...) " bg, attr_list, special
  let l:fg = copy(a:fg)
  let l:bg = get(a:, 1, ['NONE', 'NONE'])

  let l:attr_list = filter(get(a:, 2, ['NONE']), 'type(v:val) == 1')
  let l:attrs = len(l:attr_list) > 0 ? join(l:attr_list, ',') : 'NONE'

  " If the UI does not have full support for special attributes (like underline and
  " undercurl) and the highlight does not explicitly set the foreground color,
  " make the foreground the same as the attribute color to ensure the user will
  " get some highlight if the attribute is not supported. The default behavior
  " is to assume that terminals do not have full support, but the user can set
  " the global variable `g:dracula_full_special_attrs_support` explicitly if the
  " default behavior is not desirable.
  let l:special = get(a:, 3, ['NONE', 'NONE'])
  if l:special[0] !=# 'NONE' && l:fg[0] ==# 'NONE' && !g:dracula_full_special_attrs_support
    let l:fg[0] = l:special[0]
    let l:fg[1] = l:special[1]
  endif

  let l:hl_string = [
        \ 'highlight', a:scope,
        \ 'guifg=' . l:fg[0], 'ctermfg=' . l:fg[1],
        \ 'guibg=' . l:bg[0], 'ctermbg=' . l:bg[1],
        \ 'gui=' . l:attrs, 'cterm=' . l:attrs,
        \ 'guisp=' . l:special[0],
        \]

  execute join(l:hl_string, ' ')
endfunction

"}}}2
" Dracula Highlight Groups: {{{2

call s:h('DraculaBgLight', s:none, s:bglight)
call s:h('DraculaBgLighter', s:none, s:bglighter)
call s:h('DraculaBgDark', s:none, s:bgdark)
call s:h('DraculaBgDarker', s:none, s:bgdarker)

call s:h('DraculaFg', s:fg)
call s:h('DraculaFgUnderline', s:fg, s:none, [s:attrs.underline])
call s:h('DraculaFgBold', s:fg, s:none, [s:attrs.bold])
call s:h('DraculaFgStrikethrough', s:fg, s:none, [s:attrs.strikethrough])

call s:h('DraculaComment', s:comment)
call s:h('DraculaCommentBold', s:comment, s:none, [s:attrs.bold])

call s:h('DraculaSelection', s:none, s:selection)

call s:h('DraculaSubtle', s:subtle)

call s:h('DraculaCyan', s:cyan)
call s:h('DraculaCyanItalic', s:cyan, s:none, [s:attrs.italic])

call s:h('DraculaGreen', s:green)
call s:h('DraculaGreenBold', s:green, s:none, [s:attrs.bold])
call s:h('DraculaGreenItalic', s:green, s:none, [s:attrs.italic])
call s:h('DraculaGreenItalicUnderline', s:green, s:none, [s:attrs.italic, s:attrs.underline])

call s:h('DraculaOrange', s:orange)
call s:h('DraculaOrangeBold', s:orange, s:none, [s:attrs.bold])
call s:h('DraculaOrangeItalic', s:orange, s:none, [s:attrs.italic])
call s:h('DraculaOrangeBoldItalic', s:orange, s:none, [s:attrs.bold, s:attrs.italic])
call s:h('DraculaOrangeInverse', s:bg, s:orange)

call s:h('DraculaPink', s:pink)
call s:h('DraculaPinkItalic', s:pink, s:none, [s:attrs.italic])

call s:h('DraculaPurple', s:purple)
call s:h('DraculaPurpleBold', s:purple, s:none, [s:attrs.bold])
call s:h('DraculaPurpleItalic', s:purple, s:none, [s:attrs.italic])

call s:h('DraculaRed', s:red)
call s:h('DraculaRedInverse', s:fg, s:red)

call s:h('DraculaYellow', s:yellow)
call s:h('DraculaYellowItalic', s:yellow, s:none, [s:attrs.italic])

call s:h('DraculaError', s:red, s:none, [], s:red)

call s:h('DraculaErrorLine', s:none, s:none, [s:attrs.undercurl], s:red)
call s:h('DraculaWarnLine', s:none, s:none, [s:attrs.undercurl], s:orange)
call s:h('DraculaInfoLine', s:none, s:none, [s:attrs.undercurl], s:cyan)

call s:h('DraculaTodo', s:cyan, s:none, [s:attrs.bold, s:attrs.inverse])
call s:h('DraculaSearch', s:green, s:none, [s:attrs.inverse])
call s:h('DraculaBoundary', s:comment, s:bgdark)
call s:h('DraculaWinSeparator', s:comment, s:bgdark)
call s:h('DraculaLink', s:cyan, s:none, [s:attrs.underline])

if g:dracula_high_contrast_diff
  call s:h('DraculaDiffChange', s:yellow, s:purple)
  call s:h('DraculaDiffDelete', s:bgdark, s:red)
else
  call s:h('DraculaDiffChange', s:orange, s:none)
  call s:h('DraculaDiffDelete', s:red, s:bgdark)
endif

call s:h('DraculaDiffText', s:bg, s:orange)
call s:h('DraculaInlayHint', s:comment, s:bgdark)

" }}}2

" }}}
" User Interface: {{{

set background=dark

" Required as some plugins will overwrite
call s:h('Normal', s:fg, g:dracula_colorterm || has('gui_running') ? s:bg : s:none )
call s:h('StatusLine', s:none, s:bglighter, [s:attrs.bold])
call s:h('StatusLineNC', s:none, s:bglight)
call s:h('StatusLineTerm', s:none, s:bglighter, [s:attrs.bold])
call s:h('StatusLineTermNC', s:none, s:bglight)
call s:h('WildMenu', s:bg, s:purple, [s:attrs.bold])
call s:h('CursorLine', s:none, s:subtle)

hi! link ColorColumn  DraculaBgDark
hi! link CursorColumn CursorLine
hi! link CursorLineNr DraculaYellow
hi! link DiffAdd      DraculaGreen
hi! link DiffAdded    DiffAdd
hi! link DiffChange   DraculaDiffChange
hi! link DiffDelete   DraculaDiffDelete
hi! link DiffRemoved  DiffDelete
hi! link DiffText     DraculaDiffText
hi! link Directory    DraculaPurpleBold
hi! link ErrorMsg     DraculaRedInverse
hi! link FoldColumn   DraculaSubtle
hi! link Folded       DraculaBoundary
hi! link IncSearch    DraculaOrangeInverse
call s:h('LineNr', s:comment)
hi! link MoreMsg      DraculaFgBold
hi! link NonText      DraculaSubtle
hi! link Pmenu        DraculaBgDark
hi! link PmenuSbar    DraculaBgDark
hi! link PmenuSel     DraculaSelection
hi! link PmenuThumb   DraculaSelection
call s:h('PmenuMatch', s:cyan, s:bgdark)
call s:h('PmenuMatchSel', s:cyan, s:selection)
hi! link Question     DraculaFgBold
hi! link Search       DraculaSearch
call s:h('SignColumn', s:comment)
hi! link TabLine      DraculaBoundary
hi! link TabLineFill  DraculaBgDark
hi! link TabLineSel   Normal
hi! link Title        DraculaGreenBold
hi! link VertSplit    DraculaWinSeparator
hi! link Visual       DraculaSelection
hi! link VisualNOS    Visual
hi! link WarningMsg   DraculaOrangeInverse

" }}}
" Syntax: {{{

" Required as some plugins will overwrite
call s:h('MatchParen', s:green, s:none, [s:attrs.underline])
call s:h('Conceal', s:cyan, s:none)

" Neovim uses SpecialKey for escape characters only. Vim uses it for that, plus whitespace.
if has('nvim')
  hi! link SpecialKey DraculaRed
  hi! link LspReferenceText DraculaSelection
  hi! link LspReferenceRead DraculaSelection
  hi! link LspReferenceWrite DraculaSelection
  " Link old 'LspDiagnosticsDefault*' hl groups
  " for backward compatibility with neovim v0.5.x
  hi! link LspDiagnosticsDefaultInformation DiagnosticInfo
  hi! link LspDiagnosticsDefaultHint DiagnosticHint
  hi! link LspDiagnosticsDefaultError DiagnosticError
  hi! link LspDiagnosticsDefaultWarning DiagnosticWarn
  hi! link LspDiagnosticsUnderlineError DiagnosticUnderlineError
  hi! link LspDiagnosticsUnderlineHint DiagnosticUnderlineHint
  hi! link LspDiagnosticsUnderlineInformation DiagnosticUnderlineInfo
  hi! link LspDiagnosticsUnderlineWarning DiagnosticUnderlineWarn
  hi! link LspInlayHint DraculaInlayHint

  hi! link DiagnosticInfo DraculaCyan
  hi! link DiagnosticHint DraculaCyan
  hi! link DiagnosticError DraculaError
  hi! link DiagnosticWarn DraculaOrange
  hi! link DiagnosticUnderlineError DraculaErrorLine
  hi! link DiagnosticUnderlineHint DraculaInfoLine
  hi! link DiagnosticUnderlineInfo DraculaInfoLine
  hi! link DiagnosticUnderlineWarn DraculaWarnLine

  hi! link WinSeparator DraculaWinSeparator
  hi! link NormalFloat Pmenu

  if has('nvim-0.9')
    hi! link  @lsp.type.class DraculaCyan
    hi! link  @lsp.type.decorator DraculaGreen
    hi! link  @lsp.type.enum DraculaCyan
    hi! link  @lsp.type.enumMember DraculaPurple
    hi! link  @lsp.type.function DraculaGreen
    hi! link  @lsp.type.interface DraculaCyan
    hi! link  @lsp.type.macro DraculaCyan
    hi! link  @lsp.type.method DraculaGreen
    hi! link  @lsp.type.namespace DraculaCyan
    hi! link  @lsp.type.parameter DraculaOrangeItalic
    hi! link  @lsp.type.property DraculaOrange
    hi! link  @lsp.type.struct DraculaCyan
    hi! link  @lsp.type.type DraculaCyanItalic
    hi! link  @lsp.type.typeParameter DraculaPink
    hi! link  @lsp.type.variable DraculaFg
  endif
else
  hi! link SpecialKey DraculaPink
endif

hi! link Comment DraculaComment
hi! link Underlined DraculaFgUnderline
hi! link Todo DraculaTodo

hi! link Error DraculaError
hi! link SpellBad DraculaErrorLine
hi! link SpellLocal DraculaWarnLine
hi! link SpellCap DraculaInfoLine
hi! link SpellRare DraculaInfoLine

hi! link Constant DraculaPurple
hi! link String DraculaYellow
hi! link Character DraculaPink
hi! link Number Constant
hi! link Boolean Constant
hi! link Float Constant

hi! link Identifier DraculaFg
hi! link Function DraculaGreen

hi! link Statement DraculaPink
hi! link Conditional DraculaPink
hi! link Repeat DraculaPink
hi! link Label DraculaPink
hi! link Operator DraculaPink
hi! link Keyword DraculaPink
hi! link Exception DraculaPink

hi! link PreProc DraculaPink
hi! link Include DraculaPink
hi! link Define DraculaPink
hi! link Macro DraculaPink
hi! link PreCondit DraculaPink
hi! link StorageClass DraculaPink
hi! link Structure DraculaPink
hi! link Typedef DraculaPink

hi! link Type DraculaCyanItalic

hi! link Delimiter DraculaFg

hi! link Special DraculaPink
hi! link SpecialComment DraculaCyanItalic
hi! link Tag DraculaCyan
hi! link helpHyperTextJump DraculaLink
hi! link helpCommand DraculaPurple
hi! link helpExample DraculaGreen
hi! link helpBacktick Special

" }}}

" Languages: {{{

" CSS: {{{
hi! link cssAttrComma         Delimiter
hi! link cssAttrRegion        DraculaPink
hi! link cssAttributeSelector DraculaGreenItalic
hi! link cssBraces            Delimiter
hi! link cssFunctionComma     Delimiter
hi! link cssNoise             DraculaPink
hi! link cssProp              DraculaCyan
hi! link cssPseudoClass       DraculaPink
hi! link cssPseudoClassId     DraculaGreenItalic
hi! link cssUnitDecorators    DraculaPink
hi! link cssVendor            DraculaGreenItalic
" }}}

" Git Commit: {{{
" The following two are misnomers. Colors are correct.
hi! link diffFile    DraculaGreen
hi! link diffNewFile DraculaRed

hi! link diffAdded   DraculaGreen
hi! link diffLine    DraculaCyanItalic
hi! link diffRemoved DraculaRed
" }}}

" HTML: {{{
hi! link htmlTag         DraculaFg
hi! link htmlArg         DraculaGreenItalic
hi! link htmlTitle       DraculaFg
hi! link htmlH1          DraculaFg
hi! link htmlSpecialChar DraculaPurple
" }}}

" JavaScript: {{{
hi! link javaScriptBraces   Delimiter
hi! link javaScriptNumber   Constant
hi! link javaScriptNull     Constant
hi! link javaScriptFunction Keyword

" pangloss/vim-javascript
hi! link jsArrowFunction           Operator
hi! link jsBuiltins                DraculaCyan
hi! link jsClassDefinition         DraculaCyan
hi! link jsClassMethodType         Keyword
hi! link jsDestructuringAssignment DraculaOrangeItalic
hi! link jsDocParam                DraculaOrangeItalic
hi! link jsDocTags                 Keyword
hi! link jsDocType                 Type
hi! link jsDocTypeBrackets         DraculaCyan
hi! link jsFuncArgOperator         Operator
hi! link jsFuncArgs                DraculaOrangeItalic
hi! link jsFunction                Keyword
hi! link jsNull                    Constant
hi! link jsObjectColon             DraculaPink
hi! link jsSuper                   DraculaPurpleItalic
hi! link jsTemplateBraces          Special
hi! link jsThis                    DraculaPurpleItalic
hi! link jsUndefined               Constant

" maxmellon/vim-jsx-pretty
hi! link jsxTag             Keyword
hi! link jsxTagName         Keyword
hi! link jsxComponentName   Type
hi! link jsxCloseTag        Type
hi! link jsxAttrib          DraculaGreenItalic
hi! link jsxCloseString     Identifier
hi! link jsxOpenPunct       Identifier
" }}}

" JSON: {{{
hi! link jsonKeyword      DraculaCyan
hi! link jsonKeywordMatch DraculaPink
" }}}

" Lua: {{{
hi! link luaFunc  DraculaCyan
hi! link luaTable DraculaFg

" tbastos/vim-lua
hi! link luaBraces       DraculaFg
hi! link luaBuiltIn      Constant
hi! link luaDocTag       Keyword
hi! link luaErrHand      DraculaCyan
hi! link luaFuncArgName  DraculaOrangeItalic
hi! link luaFuncCall     Function
hi! link luaLocal        Keyword
hi! link luaSpecialTable Constant
hi! link luaSpecialValue DraculaCyan
" }}}

" Markdown: {{{
hi! link markdownBlockquote        DraculaCyan
hi! link markdownBold              DraculaOrangeBold
hi! link markdownBoldItalic        DraculaOrangeBoldItalic
hi! link markdownCodeBlock         DraculaGreen
hi! link markdownCode              DraculaGreen
hi! link markdownCodeDelimiter     DraculaGreen
hi! link markdownH1                DraculaPurpleBold
hi! link markdownH2                markdownH1
hi! link markdownH3                markdownH1
hi! link markdownH4                markdownH1
hi! link markdownH5                markdownH1
hi! link markdownH6                markdownH1
hi! link markdownHeadingDelimiter  markdownH1
hi! link markdownHeadingRule       markdownH1
hi! link markdownItalic            DraculaYellowItalic
hi! link markdownLinkText          DraculaPink
hi! link markdownListMarker        DraculaCyan
hi! link markdownOrderedListMarker DraculaCyan
hi! link markdownRule              DraculaComment
hi! link markdownUrl               DraculaLink

" plasticboy/vim-markdown
hi! link htmlBold       DraculaOrangeBold
hi! link htmlBoldItalic DraculaOrangeBoldItalic
hi! link htmlH1         DraculaPurpleBold
hi! link htmlItalic     DraculaYellowItalic
hi! link mkdBlockquote  DraculaYellowItalic
hi! link mkdBold        DraculaOrangeBold
hi! link mkdBoldItalic  DraculaOrangeBoldItalic
hi! link mkdCode        DraculaGreen
hi! link mkdCodeEnd     DraculaGreen
hi! link mkdCodeStart   DraculaGreen
hi! link mkdHeading     DraculaPurpleBold
hi! link mkdInlineUrl   DraculaLink
hi! link mkdItalic      DraculaYellowItalic
hi! link mkdLink        DraculaPink
hi! link mkdListItem    DraculaCyan
hi! link mkdRule        DraculaComment
hi! link mkdUrl         DraculaLink
" }}}

" OCaml: {{{
hi! link ocamlModule  Type
hi! link ocamlModPath Normal
hi! link ocamlLabel   DraculaOrangeItalic
" }}}

" Perl: {{{
" Regex
hi! link perlMatchStartEnd       DraculaRed

" Builtin functions
hi! link perlOperator            DraculaCyan
hi! link perlStatementFiledesc   DraculaCyan
hi! link perlStatementFiles      DraculaCyan
hi! link perlStatementFlow       DraculaCyan
hi! link perlStatementHash       DraculaCyan
hi! link perlStatementIOfunc     DraculaCyan
hi! link perlStatementIPC        DraculaCyan
hi! link perlStatementList       DraculaCyan
hi! link perlStatementMisc       DraculaCyan
hi! link perlStatementNetwork    DraculaCyan
hi! link perlStatementNumeric    DraculaCyan
hi! link perlStatementProc       DraculaCyan
hi! link perlStatementPword      DraculaCyan
hi! link perlStatementRegexp     DraculaCyan
hi! link perlStatementScalar     DraculaCyan
hi! link perlStatementSocket     DraculaCyan
hi! link perlStatementTime       DraculaCyan
hi! link perlStatementVector     DraculaCyan

" Highlighting for quoting constructs, tied to existing option in vim-perl
if get(g:, 'perl_string_as_statement', 0)
  hi! link perlStringStartEnd DraculaRed
endif

" Signatures
hi! link perlSignature           DraculaOrangeItalic
hi! link perlSubPrototype        DraculaOrangeItalic

" Hash keys
hi! link perlVarSimpleMemberName DraculaPurple
" }}}

" PHP: {{{
hi! link phpClass           Type
hi! link phpClasses         Type
hi! link phpDocTags         DraculaCyanItalic
hi! link phpFunction        Function
hi! link phpParent          Normal
hi! link phpSpecialFunction DraculaCyan
" }}}

" PlantUML: {{{
hi! link plantumlClassPrivate              SpecialKey
hi! link plantumlClassProtected            DraculaOrange
hi! link plantumlClassPublic               Function
hi! link plantumlColonLine                 String
hi! link plantumlDirectedOrVerticalArrowLR Constant
hi! link plantumlDirectedOrVerticalArrowRL Constant
hi! link plantumlHorizontalArrow           Constant
hi! link plantumlSkinParamKeyword          DraculaCyan
hi! link plantumlTypeKeyword               Keyword
" }}}

" PureScript: {{{
hi! link purescriptModule Type
hi! link purescriptImport DraculaCyan
hi! link purescriptImportAs DraculaCyan
hi! link purescriptOperator Operator
hi! link purescriptBacktick Operator
" }}}

" Python: {{{
hi! link pythonBuiltinObj    Type
hi! link pythonBuiltinObject Type
hi! link pythonBuiltinType   Type
hi! link pythonClassVar      DraculaPurpleItalic
hi! link pythonExClass       Type
hi! link pythonNone          Type
hi! link pythonRun           Comment
" }}}

" reStructuredText: {{{
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
" }}}

" Ruby: {{{
if ! exists('g:ruby_operators')
    let g:ruby_operators=1
endif

hi! link rubyBlockArgument          DraculaOrangeItalic
hi! link rubyBlockParameter         DraculaOrangeItalic
hi! link rubyCurlyBlock             DraculaPink
hi! link rubyGlobalVariable         DraculaPurple
hi! link rubyInstanceVariable       DraculaPurpleItalic
hi! link rubyInterpolationDelimiter DraculaPink
hi! link rubyRegexpDelimiter        DraculaRed
hi! link rubyStringDelimiter        DraculaYellow
" }}}

" Rust: {{{
hi! link rustCommentLineDoc Comment
" }}}

" Sass: {{{
hi! link sassClass                  cssClassName
hi! link sassClassChar              cssClassNameDot
hi! link sassId                     cssIdentifier
hi! link sassIdChar                 cssIdentifier
hi! link sassInterpolationDelimiter DraculaPink
hi! link sassMixinName              Function
hi! link sassProperty               cssProp
hi! link sassVariableAssignment     Operator
" }}}

" Shell: {{{
hi! link shCommandSub NONE
hi! link shEscape     DraculaRed
hi! link shParen      NONE
hi! link shParenError NONE
" }}}

" Tex: {{{
hi! link texBeginEndName  DraculaOrangeItalic
hi! link texBoldItalStyle DraculaOrangeBoldItalic
hi! link texBoldStyle     DraculaOrangeBold
hi! link texInputFile     DraculaOrangeItalic
hi! link texItalStyle     DraculaYellowItalic
hi! link texLigature      DraculaPurple
hi! link texMath          DraculaPurple
hi! link texMathMatcher   DraculaPurple
hi! link texMathSymbol    DraculaPurple
hi! link texSpecialChar   DraculaPurple
hi! link texSubscripts    DraculaPurple
hi! link texTitle         DraculaFgBold
" }}}

" Typescript: {{{
hi! link typescriptAliasDeclaration       Type
hi! link typescriptArrayMethod            Function
hi! link typescriptArrowFunc              Operator
hi! link typescriptArrowFuncArg           DraculaOrangeItalic
hi! link typescriptAssign                 Operator
hi! link typescriptBOMWindowProp          Constant
hi! link typescriptBinaryOp               Operator
hi! link typescriptBraces                 Delimiter
hi! link typescriptCall                   typescriptArrowFuncArg
hi! link typescriptClassHeritage          Type
hi! link typescriptClassName              Type
hi! link typescriptDateMethod             DraculaCyan
hi! link typescriptDateStaticMethod       Function
hi! link typescriptDecorator              DraculaGreenItalic
hi! link typescriptDefaultParam           Operator
hi! link typescriptES6SetMethod           DraculaCyan
hi! link typescriptEndColons              Delimiter
hi! link typescriptEnum                   Type
hi! link typescriptEnumKeyword            Keyword
hi! link typescriptFuncComma              Delimiter
hi! link typescriptFuncKeyword            Keyword
hi! link typescriptFuncType               DraculaOrangeItalic
hi! link typescriptFuncTypeArrow          Operator
hi! link typescriptGlobal                 Type
hi! link typescriptGlobalMethod           DraculaCyan
hi! link typescriptGlobalObjects          Type
hi! link typescriptIdentifier             DraculaPurpleItalic
hi! link typescriptInterfaceHeritage      Type
hi! link typescriptInterfaceName          Type
hi! link typescriptInterpolationDelimiter Keyword
hi! link typescriptKeywordOp              Keyword
hi! link typescriptLogicSymbols           Operator
hi! link typescriptMember                 Identifier
hi! link typescriptMemberOptionality      Special
hi! link typescriptObjectColon            Special
hi! link typescriptObjectLabel            Identifier
hi! link typescriptObjectSpread           Operator
hi! link typescriptOperator               Operator
hi! link typescriptParamImpl              DraculaOrangeItalic
hi! link typescriptParens                 Delimiter
hi! link typescriptPredefinedType         Type
hi! link typescriptRestOrSpread           Operator
hi! link typescriptTernaryOp              Operator
hi! link typescriptTypeAnnotation         Special
hi! link typescriptTypeCast               Operator
hi! link typescriptTypeParameter          DraculaOrangeItalic
hi! link typescriptTypeReference          Type
hi! link typescriptUnaryOp                Operator
hi! link typescriptVariable               Keyword

hi! link tsxAttrib           DraculaGreenItalic
hi! link tsxEqual            Operator
hi! link tsxIntrinsicTagName Keyword
hi! link tsxTagName          Type
" }}}

" Vim: {{{
hi! link vimAutoCmdSfxList     Type
hi! link vimAutoEventList      Type
hi! link vimEnvVar             Constant
hi! link vimFunction           Function
hi! link vimHiBang             Keyword
hi! link vimOption             Type
hi! link vimSetMod             Keyword
hi! link vimSetSep             Delimiter
hi! link vimUserAttrbCmpltFunc Function
hi! link vimUserFunc           Function
" }}}

" XML: {{{
hi! link xmlAttrib  DraculaGreenItalic
hi! link xmlEqual   Operator
hi! link xmlTag     Delimiter
hi! link xmlTagName Statement

" Fixes missing highlight over end tags
syn region xmlTagName
	\ matchgroup=xmlTag start=+</[^ /!?<>"']\@=+
	\ matchgroup=xmlTag end=+>+
" }}}

" YAML: {{{
hi! link yamlAlias           DraculaGreenItalicUnderline
hi! link yamlAnchor          DraculaPinkItalic
hi! link yamlBlockMappingKey DraculaCyan
hi! link yamlFlowCollection  DraculaPink
hi! link yamlFlowIndicator   Delimiter
hi! link yamlNodeTag         DraculaPink
hi! link yamlPlainScalar     DraculaYellow
" }}}

" }}}

" Plugins: {{{

" junegunn/fzf {{{
if ! exists('g:fzf_colors')
  let g:fzf_colors = {
    \ 'fg':      ['fg', 'Normal'],
    \ 'bg':      ['bg', 'Normal'],
    \ 'hl':      ['fg', 'Search'],
    \ 'fg+':     ['fg', 'Normal'],
    \ 'bg+':     ['bg', 'Normal'],
    \ 'hl+':     ['fg', 'DraculaOrange'],
    \ 'info':    ['fg', 'DraculaPurple'],
    \ 'border':  ['fg', 'Ignore'],
    \ 'prompt':  ['fg', 'DraculaGreen'],
    \ 'pointer': ['fg', 'Exception'],
    \ 'marker':  ['fg', 'Keyword'],
    \ 'spinner': ['fg', 'Label'],
    \ 'header':  ['fg', 'Comment'],
    \}
endif
" }}}

" dense-analysis/ale {{{
hi! link ALEError              DraculaErrorLine
hi! link ALEWarning            DraculaWarnLine
hi! link ALEInfo               DraculaInfoLine

hi! link ALEErrorSign          DraculaRed
hi! link ALEWarningSign        DraculaOrange
hi! link ALEInfoSign           DraculaCyan

hi! link ALEVirtualTextError   Comment
hi! link ALEVirtualTextWarning Comment
" }}}

" ctrlpvim/ctrlp.vim: {{{
hi! link CtrlPMatch     IncSearch
hi! link CtrlPBufferHid Normal
" }}}

" airblade/vim-gitgutter {{{
hi! link GitGutterAdd    DiffAdd
hi! link GitGutterChange DiffChange
hi! link GitGutterDelete DiffDelete
" }}}

" Neovim-only plugins {{{
if has('nvim')

  " nvim-treesitter/nvim-treesitter: {{{
  " The nvim-treesitter library defines many global highlight groups that are
  " linked to the regular vim syntax highlight groups. We only need to redefine
  " those highlight groups when the defaults do not match the dracula
  " specification.
  " https://github.com/nvim-treesitter/nvim-treesitter/blob/master/plugin/nvim-treesitter.vim

  " deprecated TS* highlight groups
  " see https://github.com/nvim-treesitter/nvim-treesitter/pull/3656
  " # Misc
  hi! link TSPunctSpecial Special
  " # Constants
  hi! link TSConstMacro Macro
  hi! link TSStringEscape Character
  hi! link TSSymbol DraculaPurple
  hi! link TSAnnotation DraculaYellow
  hi! link TSAttribute DraculaGreenItalic
  " # Functions
  hi! link TSFuncBuiltin DraculaCyan
  hi! link TSFuncMacro Function
  hi! link TSParameter DraculaOrangeItalic
  hi! link TSParameterReference DraculaOrange
  hi! link TSField DraculaOrange
  hi! link TSConstructor DraculaCyan
  " # Keywords
  hi! link TSLabel DraculaPurpleItalic
  " # Variable
  hi! link TSVariableBuiltin DraculaPurpleItalic
  " # Text
  hi! link TSStrong DraculaFgBold
  hi! link TSEmphasis DraculaFg
  hi! link TSUnderline Underlined
  hi! link TSTitle DraculaYellow
  hi! link TSLiteral DraculaYellow
  hi! link TSURI DraculaYellow
  " HTML and JSX tag attributes. By default, this group is linked to TSProperty,
  " which in turn links to Identifer (white).
  hi! link TSTagAttribute DraculaGreenItalic

  if has('nvim-0.8.1')
    " # Misc
    hi! link @punctuation.delimiter Delimiter
    hi! link @punctuation.bracket DraculaFg
    hi! link @punctuation.special Special
    hi! link @punctuation Delimiter
    " # Constants
    hi! link @constant Constant
    hi! link @constant.builtin Constant
    hi! link @constant.macro Macro
    hi! link @string.regex @string.special
    hi! link @string.escape @string.special
    hi! link @string String
    hi! link @string.regexp @string.special
    hi! link @string.special SpecialChar
    hi! link @string.special.symbol DraculaPurple
    hi! link @string.special.url Underlined
    hi! link @symbol DraculaPurple
    hi! link @annotation DraculaYellow
    hi! link @attribute DraculaGreenItalic
    hi! link @namespace Structure
    hi! link @module Structure
    hi! link @module.builtin Special
    " # Functions
    hi! link @function.builtin DraculaCyan
    hi! link @funcion.macro Function
    hi! link @function Function
    hi! link @parameter DraculaOrangeItalic
    hi! link @parameter.reference DraculaOrange
    hi! link @field DraculaOrange
    hi! link @property DraculaFg
    hi! link @constructor DraculaCyan
    " # Keywords
    hi! link @label DraculaPurpleItalic
    hi! link @keyword.function DraculaPink
    hi! link @keyword.operator Operator
    hi! link @keyword Keyword
    hi! link @exception DraculaPurple
    hi! link @operator Operator
    " # Types
    hi! link @type Type
    hi! link @type.builtin Special
    hi! link @character Character
    hi! link @character.special SpecialChar
    hi! link @boolean Boolean
    hi! link @number Number
    hi! link @number.float Float
    " # Variable
    hi! link @variable DraculaFg
    hi! link @variable.builtin DraculaPurpleItalic
    hi! link @variable.parameter DraculaOrangeItalic
    hi! link @variable.member  DraculaOrange
    " # Text
    hi! link @text DraculaFg
    hi! link @text.strong DraculaFgBold
    hi! link @text.emphasis DraculaFg
    hi! link @text.underline Underlined
    hi! link @text.title DraculaYellow
    hi! link @text.literal DraculaYellow
    hi! link @text.uri DraculaYellow
    hi! link @text.diff.add DiffAdd
    hi! link @text.diff.delete DiffDelete

    hi! link @markup.strong DraculaFgBold
    hi! link @markup.italic DraculaFgItalic
    hi! link @markup.strikethrough DraculaFgStrikethrough
    hi! link @markup.underline Underlined

    hi! link @markup Special
    hi! link @markup.heading DraculaYellow
    hi! link @markup.link Underlined
    hi! link @markup.link.uri DraculaYellow
    hi! link @markup.link.label SpecialChar
    hi! link @markup.raw DraculaYellow
    hi! link @markup.list Special

    hi! link @comment Comment
    hi! link @comment.error DiagnosticError
    hi! link @comment.warning DiagnosticWarn
    hi! link @comment.note DiagnosticInfo
    hi! link @comment.todo Todo

    hi! link @diff.plus Added
    hi! link @diff.minus Removed
    hi! link @diff.delta Changed

    " # Tags
    hi! link @tag DraculaCyan
    hi! link @tag.delimiter DraculaFg
    " HTML and JSX tag attributes. By default, this group is linked to TSProperty,
    " which in turn links to Identifer (white).
    hi! link @tag.attribute DraculaGreenItalic
  endif
  " }}}

  " hrsh7th/nvim-cmp {{{
  hi! link CmpItemAbbrDeprecated DraculaError

  hi! link CmpItemAbbrMatch DraculaCyan
  hi! link CmpItemAbbrMatchFuzzy DraculaCyan

  hi! link CmpItemKindText DraculaFg
  hi! link CmpItemKindMethod Function
  hi! link CmpItemKindFunction Function
  hi! link CmpItemKindConstructor DraculaCyan
  hi! link CmpItemKindField DraculaOrange
  hi! link CmpItemKindVariable DraculaPurpleItalic
  hi! link CmpItemKindClass DraculaCyan
  hi! link CmpItemKindInterface DraculaCyan
  hi! link CmpItemKindModule DraculaYellow
  hi! link CmpItemKindProperty DraculaPink
  hi! link CmpItemKindUnit DraculaFg
  hi! link CmpItemKindValue DraculaYellow
  hi! link CmpItemKindEnum DraculaPink
  hi! link CmpItemKindKeyword DraculaPink
  hi! link CmpItemKindSnippet DraculaFg
  hi! link CmpItemKindColor DraculaYellow
  hi! link CmpItemKindFile DraculaYellow
  hi! link CmpItemKindReference DraculaOrange
  hi! link CmpItemKindFolder DraculaYellow
  hi! link CmpItemKindEnumMember DraculaPurple
  hi! link CmpItemKindConstant DraculaPurple
  hi! link CmpItemKindStruct DraculaPink
  hi! link CmpItemKindEvent DraculaFg
  hi! link CmpItemKindOperator DraculaPink
  hi! link CmpItemKindTypeParameter DraculaCyan

  hi! link CmpItemMenu Comment
  " }}}

  " lewis6991/gitsigns.nvim {{{
  hi! link GitSignsAdd      DiffAdd
  hi! link GitSignsAddLn    DiffAdd
  hi! link GitSignsAddNr    DiffAdd
  hi! link GitSignsChange   DiffChange
  hi! link GitSignsChangeLn DiffChange
  hi! link GitSignsChangeNr DiffChange

  hi! link GitSignsDelete   DraculaRed
  hi! link GitSignsDeleteLn DraculaRed
  hi! link GitSignsDeleteNr DraculaRed
  " }}}

endif
" }}}

" }}}

" vim: fdm=marker ts=2 sts=2 sw=2 fdl=0 et:
