if dracula#should_abort()
  finish
endif

" Fzf: {{{
if exists('g:loaded_fzf') && ! exists('g:fzf_colors')
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
"}}}
" ALE: {{{
if exists('g:ale_enabled')
  hi! link ALEError              DraculaErrorLine
  hi! link ALEWarning            DraculaWarnLine
  hi! link ALEInfo               DraculaInfoLine

  hi! link ALEErrorSign          DraculaRed
  hi! link ALEWarningSign        DraculaOrange
  hi! link ALEInfoSign           DraculaCyan

  hi! link ALEVirtualTextError   Comment
  hi! link ALEVirtualTextWarning Comment
endif
" }}}
" CtrlP: {{{
if exists('g:loaded_ctrlp')
  hi! link CtrlPMatch     IncSearch
  hi! link CtrlPBufferHid Normal
endif
" }}}
" GitGutter / gitsigns: {{{
if exists('g:loaded_gitgutter')
  hi! link GitGutterAdd    DiffAdd
  hi! link GitGutterChange DiffChange
  hi! link GitGutterDelete DiffDelete
endif
if has('nvim-0.5') && luaeval("pcall(require, 'gitsigns')")
  " https://github.com/lewis6991/gitsigns.nvim requires nvim > 0.5
  " has('nvim-0.5') checks >= 0.5, so this should be future-proof.
  hi! link GitSignsAdd      DiffAdd
  hi! link GitSignsAddLn    DiffAdd
  hi! link GitSignsAddNr    DiffAdd
  hi! link GitSignsChange   DiffChange
  hi! link GitSignsChangeLn DiffChange
  hi! link GitSignsChangeNr DiffChange
  hi! link GitSignsDelete   DiffDelete
  hi! link GitSignsDeleteLn DiffDelete
  hi! link GitSignsDeleteNr DiffDelete
endif
" }}}
" Tree-sitter: {{{
" The nvim-treesitter library defines many global highlight groups that are
" linked to the regular vim syntax highlight groups. We only need to redefine
" those highlight groups when the defaults do not match the dracula
" specification.
" https://github.com/nvim-treesitter/nvim-treesitter/blob/master/plugin/nvim-treesitter.vim
if exists('g:loaded_nvim_treesitter')
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
endif
" }}}
" nvim-cmp: {{{
" A completion engine plugin for neovim written in Lua.
" https://github.com/hrsh7th/nvim-cmp
if exists('g:loaded_cmp')
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
endif
" }}}

" vim: fdm=marker ts=2 sts=2 sw=2 fdl=0:
