" =============================================================================
"
" File:        mayansmoke.vim
" Description: Vim color scheme file
" Maintainer:  Jeet Sukumaran (GUI colors); Clayton Parker (cterm colors)
"
" =============================================================================

"  Initialization and Setup {{{1
" =============================================================================
set background=light
highlight clear
if exists("syntax_on")
  syntax reset
endif
let colors_name = "mayansmoke"
" }}}

"  Normal Color {{{1
" =============================================================================
hi Normal gui=NONE guifg=Black guibg=#F4F4E8
" }}}

"  Highlight Groups {{{1
" =============================================================================
" Groups (see ':help highlight-groups'):
"    ColorColumn     highlight to use with ':set colorcolumn'
"    Cursor          the character under the cursor
"    CursorIM        like Cursor, but used when in IME mode |CursorIM|
"    CursorColumn    the screen column that the cursor is in when 'cursorcolumn' is set
"    CursorLine      the screen line that the cursor is in when 'cursorline' is set
"    Directory       directory names (and other special names in listings)
"    DiffAdd         diff mode: Added line |diff.txt|
"    DiffChange      diff mode: Changed line |diff.txt|
"    DiffDelete      diff mode: Deleted line |diff.txt|
"    DiffText        diff mode: Changed text within a changed line |diff.txt|
"    ErrorMsg        error messages on the command line
"    VertSplit       the column separating vertically split windows
"    Folded          line used for closed folds
"    FoldColumn      'foldcolumn'
"    SignColumn      column where |signs| are displayed
"    IncSearch       'incsearch' highlighting; also used for the text replaced with ":s///c"
"    LineNr          Line number for ":number" and ":#" commands, and when 'number' option is set.
"    MatchParen      The character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|
"    ModeMsg         'showmode' message (e.g., "-- INSERT --")
"    MoreMsg         |more-prompt|
"    NonText         '~' and '@' at the end of the window, etc.
"    Normal          normal text
"    Pmenu           Popup menu: normal item.
"    PmenuSel        Popup menu: selected item.
"    PmenuSbar       Popup menu: scrollbar.
"    PmenuThumb      Popup menu: Thumb of the scrollbar.
"    Question        |hit-enter| prompt and yes/no questions
"    Search          Last search pattern highlighting (see 'hlsearch').
"    SpecialKey      Meta and special keys listed with ":map", text that is displayed differently from what it really is (such as tabs, spaces in listchars etc.).
"    SpellBad        Word that is not recognized by the spellchecker. |spell|
"    SpellCap        Word that should start with a capital. |spell|
"    SpellLocal      Word that is recognized by the spellchecker as one that is
"    SpellRare       Word that is recognized by the spellchecker as one that is hardly ever used. |spell|
"    StatusLine      status line of current window
"    StatusLineNC    status lines of not-current windows
"    TabLine         tab pages line, not active tab page label
"    TabLineFill     tab pages line, where there are no labels
"    TabLineSel      tab pages line, active tab page label
"    Title           titles for output from ":set all", ":autocmd" etc.
"    Visual          Visual mode selection
"    VisualNOS       Visual mode selection when vim is "Not Owning the Selection".
"    WarningMsg      warning messages
"    WildMenu        current match in 'wildmenu' completion
hi ColorColumn  guifg=NONE              guibg=#EEEEDD
hi Cursor       guifg=bg                guibg=fg                gui=NONE
if hlexists('MayanSmokeCursorLine')
    hi link CursorColumn MayanSmokeCursorLine
    hi link CursorLine MayanSmokeCursorLine
elseif exists('g:mayansmoke_cursor_line_visibility') && g:mayansmoke_cursor_line_visibility >= 2
    hi CursorColumn guifg=NONE              guibg=NavajoWhite   gui=NONE
    hi CursorLine   guifg=NONE              guibg=NavajoWhite   gui=NONE
elseif exists('g:mayansmoke_cursor_line_visibility') && g:mayansmoke_cursor_line_visibility >= 1
    hi CursorColumn guifg=NONE              guibg=white         gui=NONE
    hi CursorLine   guifg=NONE              guibg=white         gui=NONE
else
    hi CursorColumn guifg=NONE              guibg=#FFFDD0       gui=NONE
    hi CursorLine   guifg=NONE              guibg=#FFFDD0       gui=NONE
endif
hi CursorIM     guifg=bg                guibg=fg                gui=NONE
hi lCursor      guifg=bg                guibg=fg                gui=NONE
hi DiffAdd      guifg=NONE              guibg=SeaGreen1         gui=NONE
hi DiffChange   guifg=NONE              guibg=LightSkyBlue1     gui=NONE
hi DiffDelete   guifg=NONE              guibg=LightCoral        gui=NONE
hi DiffText     guifg=black             guibg=LightCyan1        gui=NONE
hi Directory    guifg=#1600FF           guibg=bg                gui=NONE
hi ErrorMsg     guifg=Red2              guibg=NONE              gui=NONE
hi FoldColumn   guifg=SteelBlue4        guibg=LightYellow2      gui=bold
hi Folded       guifg=SteelBlue4        guibg=Gainsboro      gui=italic
if hlexists('MayanSmokeSearch')
    hi link IncSearch MayanSmokeSearch
    hi link Search MayanSmokeSearch
elseif exists('g:mayansmoke_search_visibility') && g:mayansmoke_search_visibility >= 4
    hi IncSearch    guifg=white             guibg=red           gui=NONE
    hi Search       guifg=white             guibg=red           gui=NONE
elseif exists('g:mayansmoke_search_visibility') && g:mayansmoke_search_visibility == 3
    hi IncSearch    guifg=black             guibg=gold       gui=NONE
    hi Search       guifg=black             guibg=gold        gui=NONE
elseif exists('g:mayansmoke_search_visibility') && g:mayansmoke_search_visibility == 2
    hi IncSearch    guifg=white             guibg=darkorange       gui=NONE
    hi Search       guifg=white             guibg=darkorange        gui=NONE
elseif exists('g:mayansmoke_search_visibility') && g:mayansmoke_search_visibility == 0
    hi IncSearch    guifg=black             guibg=tan         gui=NONE
    hi Search       guifg=black             guibg=tan         gui=NONE
else
    hi IncSearch    guifg=black             guibg=khaki          gui=NONE
    hi Search       guifg=black             guibg=khaki          gui=NONE
endif
hi LineNr       guifg=#666677           guibg=#cccfbf    gui=NONE
hi MatchParen   guifg=black             guibg=LemonChiffon3     gui=bold
hi ModeMsg      guifg=White             guibg=tomato1           gui=bold
hi MoreMsg      guifg=SeaGreen4         guibg=bg                gui=bold
hi NonText      guifg=LightCyan3        guibg=bg                gui=bold

hi Pmenu        guifg=Orange4           guibg=LightYellow3      gui=NONE
hi PmenuSel     guifg=ivory2            guibg=NavajoWhite4      gui=bold
hi PmenuSbar    guifg=White             guibg=#999666        gui=NONE
hi PmenuThumb   guifg=White             guibg=#7B7939        gui=NONE

hi Question     guifg=Chartreuse4       guibg=bg                gui=bold
hi SignColumn   guifg=white             guibg=LightYellow3      gui=NONE
if hlexists('MayanSmokeSpecialKey')
    hi link SpecialKey MayanSmokeSpecialKey
elseif exists('g:mayansmoke_special_key_visibility') && g:mayansmoke_special_key_visibility >= 2
    hi SpecialKey   guifg=black         guibg=NavajoWhite       gui=NONE
elseif exists('g:mayansmoke_special_key_visibility') && g:mayansmoke_special_key_visibility == 0
    hi SpecialKey   guifg=bisque3       guibg=NONE              gui=NONE
else
    hi SpecialKey   guifg=white         guibg=ivory3            gui=NONE
endif
hi SpellBad     guisp=Firebrick2                                gui=undercurl
hi SpellCap     guisp=Blue                                      gui=undercurl
hi SpellLocal   guisp=DarkCyan                                  gui=undercurl
hi SpellRare    guisp=Magenta                                   gui=undercurl
hi StatusLine   guifg=#FFFEEE           guibg=#557788     gui=NONE
" hi StatusLineNC guifg=#EAE6E2           guibg=LightSteelBlue3    gui=italic
hi StatusLineNC guifg=#F4F4EE           guibg=#99aabb    gui=italic
hi TabLine      guifg=fg                guibg=LightGrey         gui=underline
hi TabLineFill  guifg=fg                guibg=bg                gui=reverse
hi TabLineSel   guifg=fg                guibg=bg                gui=bold
hi Title        guifg=DeepSkyBlue3      guibg=bg                gui=bold
hi VertSplit    guifg=#99aabb     guibg=#99aabb
hi Visual       guifg=white             guibg=DeepSkyBlue1      gui=NONE
hi WarningMsg   guifg=Firebrick2        guibg=bg                gui=NONE
hi WildMenu     guifg=Black             guibg=SkyBlue           gui=NONE
" }}}

" 256-Color Terminal Colors, by Clayton Parker {{{1
" =============================================================================
hi Normal cterm=NONE ctermfg=16  ctermbg=255
hi Comment      ctermfg=110
hi Constant     ctermfg=214
    hi String   ctermfg=30
    hi Boolean  ctermfg=88
hi Identifier   ctermfg=160
hi Function     ctermfg=132
hi Statement    ctermfg=21
hi Keyword      ctermfg=45
hi PreProc      ctermfg=27
hi Type         ctermfg=147
hi Special      ctermfg=64
hi Ignore       ctermfg=255
hi Error        ctermfg=196             ctermbg=255     term=none
hi Todo         ctermfg=136             ctermbg=255     cterm=NONE
hi VimError         ctermfg=160          ctermbg=16
hi VimCommentTitle  ctermfg=110
hi qfLineNr         ctermfg=16           ctermbg=46        cterm=NONE
hi pythonDecorator ctermfg=208   ctermbg=255 cterm=NONE
hi Cursor       ctermfg=255             ctermbg=16              cterm=NONE
hi CursorColumn ctermfg=NONE            ctermbg=255             cterm=NONE
hi CursorIM     ctermfg=255             ctermbg=16              cterm=NONE
hi CursorLine   ctermfg=NONE            ctermbg=254             cterm=NONE
hi lCursor      ctermfg=255             ctermbg=16              cterm=NONE
hi DiffAdd      ctermfg=16              ctermbg=48              cterm=NONE
hi DiffChange   ctermfg=16              ctermbg=153             cterm=NONE
hi DiffDelete   ctermfg=16              ctermbg=203             cterm=NONE
hi DiffText     ctermfg=16              ctermbg=226             cterm=NONE
hi Directory    ctermfg=21              ctermbg=255             cterm=NONE
hi ErrorMsg     ctermfg=160             ctermbg=NONE            cterm=NONE
hi FoldColumn   ctermfg=24              ctermbg=252             cterm=NONE
hi Folded       ctermfg=24              ctermbg=252             cterm=NONE
hi IncSearch    ctermfg=255             ctermbg=160             cterm=NONE
hi LineNr       ctermfg=253             ctermbg=110             cterm=NONE
hi NonText      ctermfg=110             ctermbg=255             cterm=NONE
hi Pmenu        ctermfg=fg              ctermbg=195             cterm=NONE
hi PmenuSbar    ctermfg=255             ctermbg=153             cterm=NONE
hi PmenuSel     ctermfg=255             ctermbg=21              cterm=NONE
hi PmenuThumb   ctermfg=111             ctermbg=255             cterm=NONE
hi SignColumn   ctermfg=110             ctermbg=254             cterm=NONE
hi Search       ctermfg=255             ctermbg=160             cterm=NONE
hi SpecialKey   ctermfg=255             ctermbg=144             cterm=NONE
hi SpellBad     ctermfg=16              ctermbg=229             cterm=NONE
hi SpellCap     ctermfg=16              ctermbg=231             cterm=NONE
hi SpellLocal   ctermfg=16              ctermbg=231             cterm=NONE
hi SpellRare    ctermfg=16              ctermbg=226             cterm=NONE
hi StatusLine   ctermfg=255             ctermbg=24              cterm=NONE
hi StatusLineNC ctermfg=253             ctermbg=110             cterm=NONE
hi Title        ctermfg=75              ctermbg=255             cterm=NONE
hi VertSplit    ctermfg=255             ctermbg=24              cterm=NONE
hi Visual       ctermfg=255             ctermbg=153             cterm=NONE
hi WildMenu     ctermfg=16              ctermbg=117             cterm=NONE

" 1}}}

"  Syntax {{{1
" =============================================================================

"  General {{{2
" -----------------------------------------------------------------------------
" Groups ('*' = major; see 'help group-name'):
"   *Comment        any comment
"   *Constant       any constant
"       String         a string constant: "this is a string"
"       Character      a character constant: 'c', '\n'
"       Number         a number constant: 234, 0xff
"       Boolean        a boolean constant: TRUE, false
"       Float          a floating point constant: 2.3e10
"   *Identifier     any variable name
"       Function       function name (also: methods for classes)
"   *Statement      any statement
"       Conditional    if, then, else, endif, switch, etc.
"       Repeat         for, do, while, etc.
"       Label          case, default, etc.
"       Operator       "sizeof", "+", "*", etc.
"       Keyword        any other keyword
"       Exception      try, catch, throw
"   *PreProc        generic Preprocessor
"       Include        preprocessor #include
"       Define         preprocessor #define
"       Macro          same as Define
"       PreCondit      preprocessor #if, #else, #endif, etc.
"   *Type           int, long, char, etc.
"       StorageClass   static, register, volatile, etc.
"       Structure      struct, union, enum, etc.
"       Typedef        A typedef
"   *Special        any special symbol
"       SpecialChar    special character in a constant
"       Tag            you can use CTRL-] on this
"       Delimiter      character that needs attention
"       SpecialComment special things inside a comment
"       Debug          debugging statements
"   *Error          any erroneous construct
"   *Todo           anything that needs extra attention
" hi Comment      guifg=#A2B5CD         guibg=NONE      gui=italic
hi Comment      guifg=#96AAC2         guibg=NONE      gui=italic
hi Constant     guifg=DarkOrange        guibg=NONE      gui=NONE
    hi String   guifg=Aquamarine4       guibg=NONE      gui=NONE
    hi Boolean  guifg=IndianRed4        guibg=NONE      gui=NONE
hi Identifier   guifg=brown3            guibg=NONE      gui=NONE
hi Function     guifg=VioletRed4        guibg=NONE      gui=NONE
hi Statement    guifg=blue1             guibg=NONE      gui=NONE
hi Keyword      guifg=DodgerBlue        guibg=NONE      gui=NONE
hi PreProc      guifg=blue1             guibg=NONE      gui=NONE
hi Type         guifg=LightSlateBlue    guibg=NONE      gui=NONE
hi Special      guifg=DarkOliveGreen4   guibg=NONE      gui=NONE
hi Ignore       guifg=bg                guibg=NONE      gui=NONE
hi Error        guifg=Red               guibg=NONE      gui=underline
hi Todo         guifg=tan4              guibg=NONE      gui=underline
" 2}}}

"  Vim {{{2
" -----------------------------------------------------------------------------
hi VimError         guifg=red            guibg=Black   gui=bold
hi VimCommentTitle  guifg=DarkSlateGray4 guibg=bg      gui=bold,italic
" 2}}}

" QuickFix {{{2
" -----------------------------------------------------------------------------

" syn match qfFileName  "^[^|]*" nextgroup=qfSeparator
" syn match qfSeparator "|" nextgroup=qfLineNr contained
" syn match qfLineNr    "[^|]*" contained contains=qfError
" syn match qfError     "error" contained
hi qfFileName  guifg=LightSkyBlue4     guibg=NONE      gui=italic
hi qfLineNr    guifg=coral             guibg=NONE      gui=bold
hi qfError     guifg=red               guibg=NONE      gui=bold
" 2}}}

" Python {{{2
" -----------------------------------------------------------------------------
hi pythonDecorator  guifg=orange3 guibg=NONE gui=bold
hi link pythonDecoratorFunction pythonDecorator
" 2}}}

" Diff {{{2
" -----------------------------------------------------------------------------
hi diffOldFile          guifg=#006666           guibg=NONE      gui=NONE
hi diffNewFile          guifg=#0088FF           guibg=NONE      gui=bold
hi diffFile             guifg=#0000FF           guibg=NONE      gui=NONE
hi link diffOnly        Constant
hi link diffIdentical   Constant
hi link diffDiffer      Constant
hi link diffBDiffer     Constant
hi link diffIsA         Constant
hi link diffNoEOL       Constant
hi link diffCommon      Constant
hi diffRemoved          guifg=#BB0000           guibg=NONE      gui=NONE
hi diffChanged          guifg=DarkSeaGreen      guibg=NONE      gui=NONE
hi diffAdded            guifg=#00AA00           guibg=NONE      gui=NONE
hi diffLine             guifg=thistle4          guibg=NONE      gui=italic
hi link diffSubname     diffLine
hi link diffComment     Comment
" 2}}}

" PHP (contributed by Ryan Kulla) {{{2
" -----------------------------------------------------------------------------
" Ryan Kulla's addition for PHP syntax highlighting (for regular/terminal vim)
hi phpConditional ctermfg=21 cterm=NONE guifg=black
hi phpIdentifier ctermfg=0 cterm=NONE guifg=black
hi phpOperator ctermfg=black cterm=NONE guifg=black
hi phpRegion ctermfg=132 cterm=NONE guifg=VioletRed4
hi phpComparison ctermfg=black cterm=NONE guifg=black
hi phpType ctermfg=darkgreen cterm=NONE guifg=darkgreen
hi phpParent ctermfg=black cterm=NONE guifg=black
hi phpMethodsVar ctermfg=132 cterm=NONE guifg=VioletRed4
hi phpStatement ctermfg=21 cterm=NONE guifg=blue
hi phpStorageClass ctermfg=21 cterm=NONE guifg=blue
hi phpStringSingle ctermfg=30 cterm=NONE guifg=Aquamarine4
hi phpStringDouble ctermfg=30 cterm=NONE guifg=Aquamarine4
hi phpFunctions ctermfg=21 cterm=NONE guifg=blue
hi phpSpecialFunction ctermfg=21 cterm=NONE guifg=blue
hi phpRepeat ctermfg=21 cterm=NONE guifg=blue
hi phpNumber ctermfg=214 cterm=bold guifg=brown
hi phpTodo ctermfg=red cterm=bold guifg=red gui=bold
hi phpDefine ctermfg=21 cterm=NONE guifg=blue
hi phpConstant ctermfg=21 cterm=NONE guifg=black
hi phpCoreConstant ctermfg=21 cterm=NONE guifg=black
hi phpMemberSelector ctermfg=black cterm=NONE guifg=black
hi phpLabel ctermfg=21 cterm=NONE guifg=blue
hi phpStructure ctermfg=black cterm=NONE guifg=black
hi phpRelation ctermfg=black cterm=NONE guifg=black
hi phpEnvVar ctermfg=black cterm=NONE guifg=black
hi phpIntVar ctermfg=0 cterm=bold guifg=black gui=bold
hi phpBoolean ctermfg=58 cterm=NONE guifg=brown
" 2}}}

" 1}}}

