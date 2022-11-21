" =============================================================================
" Filename: autoload/lightline/colorscheme/rosepine.vim
" Author: sheruost
" License: MIT License
" Last Change: 2022/05/09 23:27:50.
" =============================================================================

" Reference: https://rosepinetheme.com/palette
if lightline#colorscheme#background() ==# 'light'
  " Rosé Pine Dawn
  let s:base = [ '#faf4ed', 255 ]
  let s:surface = [ '#fffaf3', 255 ]

  let s:overlay = [ '#f2e9e1', 254 ]
  let s:highlight_m = [ '#dfdad9', 145 ]
  let s:muted = [ '#9893a5', 103 ]
  let s:subtle = [ '#797593', 102 ]

  let s:iris = [ '#907aa9', 139 ]
  let s:pine = [ '#286983', 24 ]
  let s:foam = [ '#56949f', 67 ]
  let s:rose = [ '#d7827e', 174 ]
  let s:love = [ '#b4637a', 132 ]
else
  " Rosé Pine
  let s:base = [ '#191724', 233 ]
  let s:surface = [ '#1f1d2e', 234 ]

  let s:overlay = [ '#26233a', 235 ]
  let s:highlight_m = [ '#403d52', 59 ]
  let s:muted = [ '#6e6a86', 60 ]
  let s:subtle = [ '#908caa', 103 ]

  let s:iris = [ '#c4a7e7', 182 ]
  let s:pine = [ '#31748f', 30 ]
  let s:foam = [ '#9ccfd8', 152 ]
  let s:rose = [ '#ebbcba', 217 ]
  let s:love = [ '#eb6f92', 204 ]
endif

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:base, s:pine ], [ s:subtle, s:surface ] ]
let s:p.normal.right = [ [ s:overlay, s:subtle ], [ s:muted, s:overlay ], [ s:highlight_m, s:surface ] ]
let s:p.inactive.right = [ [ s:base, s:surface ], [ s:overlay, s:base ] ]
let s:p.inactive.left =  [ [ s:overlay, s:base ], [ s:surface, s:base ] ]
let s:p.insert.left = [ [ s:base, s:foam ], [ s:subtle, s:surface ] ]
let s:p.replace.left = [ [ s:base, s:love ], [ s:subtle, s:surface ] ]
let s:p.visual.left = [ [ s:base, s:iris ], [ s:subtle, s:surface ] ]
let s:p.normal.middle = [ [ s:overlay, s:base ] ]
let s:p.inactive.middle = [ [ s:surface, s:base ] ]
let s:p.tabline.left = [ [ s:subtle, s:base ] ]
let s:p.tabline.tabsel = [ [ s:pine, s:base ] ]
let s:p.tabline.middle = [ [ s:surface, s:base ] ]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.normal.error = [ [ s:love, s:base ] ]
let s:p.normal.warning = [ [ s:rose, s:surface ] ]

let g:lightline#colorscheme#rosepine#palette = lightline#colorscheme#flatten(s:p)
