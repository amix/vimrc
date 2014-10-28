" ----------------------------------------------------------------------------
" Vim color file
" Maintainer:   John-Paul Bader <contact@smyck.org>
" Last Change:  2012 April
" License:      Beer Ware
" ----------------------------------------------------------------------------

" Reset Highlighting
hi clear
if exists("syntax_on")
  syntax reset
endif

set background=dark
set linespace=3

let g:colors_name = "smyck"

hi Normal               cterm=none ctermbg=none     ctermfg=15      gui=none        guibg=#282828   guifg=#F7F7F7
hi LineNr               cterm=none ctermbg=none     ctermfg=8       gui=none        guibg=#282828   guifg=#8F8F8F
hi StatusLine           cterm=none ctermbg=8        ctermfg=15      gui=none        guibg=#5D5D5D   guifg=#FBFBFB
hi StatusLineNC         cterm=none ctermbg=15       ctermfg=8       gui=none        guibg=#5D5D5D   guifg=#FBFBFB
hi Search               cterm=none ctermbg=6        ctermfg=15      gui=none        guibg=#2EB5C1   guifg=#F7F7F7
hi IncSearch            cterm=none ctermbg=3        ctermfg=8       gui=none        guibg=#F6DC69   guifg=#8F8F8F
hi ColumnMargin         cterm=none ctermbg=0                        gui=none        guibg=#000000
hi Error                cterm=none ctermbg=1        ctermfg=15      gui=none                        guifg=#F7F7F7
hi ErrorMsg             cterm=none ctermbg=1        ctermfg=15      gui=none                        guifg=#F7F7F7
hi Folded               cterm=none ctermbg=none     ctermfg=2       gui=none        guibg=#3B3B3B   guifg=#90AB41
hi FoldColumn           cterm=none ctermbg=8        ctermfg=2       gui=none        guibg=#3B3B3B   guifg=#90AB41
hi NonText              cterm=bold ctermbg=none     ctermfg=8       gui=bold                        guifg=#8F8F8F
hi ModeMsg              cterm=bold ctermbg=none     ctermfg=10      gui=none
hi Pmenu                cterm=none ctermbg=8        ctermfg=15      gui=none        guibg=#8F8F8F   guifg=#F7F7F7
hi PmenuSel             cterm=none ctermbg=15       ctermfg=8       gui=none        guibg=#F7F7F7   guifg=#8F8F8F
hi PmenuSbar            cterm=none ctermbg=15       ctermfg=8       gui=none        guibg=#F7F7F7   guifg=#8F8F8F
hi SpellBad             cterm=none ctermbg=1        ctermfg=15      gui=none                        guifg=#F7F7F7
hi SpellCap             cterm=none ctermbg=4        ctermfg=15      gui=none                        guifg=#F7F7F7
hi SpellRare            cterm=none ctermbg=4        ctermfg=15      gui=none                        guifg=#F7F7F7
hi SpellLocal           cterm=none ctermbg=4        ctermfg=15      gui=none                        guifg=#F7F7F7
hi Visual               cterm=none ctermbg=15       ctermfg=8       gui=none        guibg=#F7F7F7   guifg=#8F8F8F
hi Directory            cterm=none ctermbg=none     ctermfg=4       gui=none        guibg=#242424   guifg=#88CCE7
hi SpecialKey           cterm=none ctermbg=none     ctermfg=8       gui=none                        guifg=#8F8F8F
hi DiffAdd              cterm=bold ctermbg=2        ctermfg=15
hi DiffChange           cterm=bold ctermbg=4        ctermfg=15
hi DiffDelete           cterm=bold ctermbg=1        ctermfg=15
hi DiffText             cterm=bold ctermbg=3        ctermfg=8
hi MatchParen           cterm=none ctermbg=6        ctermfg=15      gui=none        guibg=#2EB5C1   guifg=#F7F7F7
hi CursorLine           cterm=none ctermbg=238      ctermfg=none    gui=none        guibg=#424242
hi CursorColumn         cterm=none ctermbg=238      ctermfg=none    gui=none        guibg=#424242
hi Title                cterm=none ctermbg=none     ctermfg=4       gui=none                        guifg=#88CCE7

" ----------------------------------------------------------------------------
" Syntax Highlighting
" ----------------------------------------------------------------------------
hi Keyword              cterm=none ctermbg=none ctermfg=10          gui=none        guifg=#D1FA71
hi Comment              cterm=none ctermbg=none ctermfg=8           gui=none        guifg=#8F8F8F
hi Delimiter            cterm=none ctermbg=none ctermfg=15          gui=none        guifg=#F7F7F7
hi Identifier           cterm=none ctermbg=none ctermfg=12          gui=none        guifg=#96D9F1
hi Structure            cterm=none ctermbg=none ctermfg=12          gui=none        guifg=#9DEEF2
hi Ignore               cterm=none ctermbg=none ctermfg=8           gui=none        guifg=bg
hi Constant             cterm=none ctermbg=none ctermfg=12          gui=none        guifg=#96D9F1
hi PreProc              cterm=none ctermbg=none ctermfg=10          gui=none        guifg=#D1FA71
hi Type                 cterm=none ctermbg=none ctermfg=12          gui=none        guifg=#96D9F1
hi Statement            cterm=none ctermbg=none ctermfg=10          gui=none        guifg=#D1FA71
hi Special              cterm=none ctermbg=none ctermfg=6           gui=none        guifg=#d7d7d7
hi String               cterm=none ctermbg=none ctermfg=3           gui=none        guifg=#F6DC69
hi Number               cterm=none ctermbg=none ctermfg=3           gui=none        guifg=#F6DC69
hi Underlined           cterm=none ctermbg=none ctermfg=magenta     gui=underline   guibg=#272727
hi Symbol               cterm=none ctermbg=none ctermfg=9           gui=none        guifg=#FAB1AB
hi Method               cterm=none ctermbg=none ctermfg=15          gui=none        guifg=#F7F7F7
hi Interpolation        cterm=none ctermbg=none ctermfg=6           gui=none        guifg=#2EB5C1

" Erlang
hi link erlangAtom            Keyword
hi link erlangBitType         Keyword

hi link rubyBeginend          Keyword
hi link rubyClass             Keyword
hi link rubyModule            Keyword
hi link rubyKeyword           Keyword
hi link rubyOperator          Method
hi link rubyIdentifier        Keyword
hi link rubyClassVariable     Symbol
hi link rubyInstanceVariable  Constant
hi link rubyGlobalVariable    Constant
hi link rubyClassVariable     Method
hi link rubyConstant          Constant
hi link rubySymbol            Symbol
hi link rubyFunction          Constant
hi link rubyControl           Keyword
hi link rubyConditional       Keyword
hi link rubyInterpolation     Interpolation
hi link rubyInterpolationDelimiter    Interpolation
hi link rubyRailsMethod       Method


