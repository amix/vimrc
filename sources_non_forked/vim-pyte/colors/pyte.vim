
set background=light

hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "pyte"

if version >= 700
  hi CursorLine guibg=#f6f6f6
  hi CursorColumn guibg=#eaeaea
  hi MatchParen guifg=white guibg=#80a090 gui=bold

  "Tabpages
  hi TabLine guifg=black guibg=#b0b8c0 gui=italic
  hi TabLineFill guifg=#9098a0
  hi TabLineSel guifg=black guibg=#f0f0f0 gui=italic,bold

  "P-Menu (auto-completion)
  hi Pmenu guifg=white guibg=#808080
  "PmenuSel
  "PmenuSbar
  "PmenuThumb
endif
"
" Html-Titles
hi Title      guifg=#202020 gui=bold
hi Underlined  guifg=#202020 gui=underline


hi Cursor    guifg=black   guibg=#b0b4b8
hi lCursor   guifg=black   guibg=white
hi LineNr    guifg=#ffffff guibg=#c0d0e0

hi Normal    guifg=#404850   guibg=#f0f0f0

hi StatusLine guifg=white guibg=#8090a0 gui=bold,italic
hi StatusLineNC guifg=#506070 guibg=#a0b0c0 gui=italic
hi VertSplit guifg=#a0b0c0 guibg=#a0b0c0 gui=NONE

" hi Folded    guifg=#708090 guibg=#c0d0e0
hi Folded    guifg=#a0a0a0 guibg=#e8e8e8 gui=italic

hi NonText   guifg=#c0c0c0 guibg=#e0e0e0
" Kommentare
hi Comment   guifg=#a0b0c0               gui=italic

" Konstanten
hi Constant  guifg=#a07040
hi String    guifg=#4070a0
hi Number    guifg=#40a070
hi Float     guifg=#70a040
"hi Statement guifg=#0070e0 gui=NONE
" Python: def and so on, html: tag-names
hi Statement  guifg=#007020 gui=bold


" HTML: arguments
hi Type       guifg=#e5a00d gui=italic
" Python: Standard exceptions, True&False
hi Structure  guifg=#007020 gui=italic
hi Function   guifg=#06287e gui=italic

hi Identifier guifg=#5b3674 gui=italic

hi Repeat      guifg=#7fbf58 gui=bold
hi Conditional guifg=#4c8f2f gui=bold

" Cheetah: #-Symbol, function-names
hi PreProc    guifg=#1060a0 gui=NONE
" Cheetah: def, for and so on, Python: Decorators
hi Define      guifg=#1060a0 gui=bold

hi Error      guifg=red guibg=white gui=bold,underline
hi Todo       guifg=#a0b0c0 guibg=NONE gui=italic,bold,underline

" Python: %(...)s - constructs, encoding
hi Special    guifg=#70a0d0 gui=italic

hi Operator   guifg=#408010

" color of <TAB>s etc...
"hi SpecialKey guifg=#d8a080 guibg=#e8e8e8 gui=italic
hi SpecialKey guifg=#d0b0b0 guibg=#f0f0f0 gui=none

" Diff
hi DiffChange guifg=NONE guibg=#e0e0e0 gui=italic,bold
hi DiffText guifg=NONE guibg=#f0c8c8 gui=italic,bold
hi DiffAdd guifg=NONE guibg=#c0e0d0 gui=italic,bold
hi DiffDelete guifg=NONE guibg=#f0e0b0 gui=italic,bold


