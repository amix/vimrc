" =============================================================================
" Filename: autoload/lightline/colorscheme/16color.vim
" Author: itchyny
" License: MIT License
" Last Change: 2014/01/02 10:04:03.
" =============================================================================
let s:base03 = [ '#808080', 8 ]
let s:base02 = [ '#000000', 0 ]
let s:base01 = [ '#00ff00', 10 ]
let s:base00 = [ '#ffff00', 11  ]
let s:base0 = [ '#0000ff', 12 ]
let s:base1 = [ '#00ffff', 14 ]
let s:base2 = [ '#c0c0c0', 7 ]
let s:base3 = [ '#ffffff', 15 ]
let s:yellow = [ '#808000', 3 ]
let s:orange = [ '#ff0000', 9 ]
let s:red = [ '#800000', 1 ]
let s:magenta = [ '#800080', 5 ]
let s:violet = [ '#ff00ff', 13 ]
let s:blue = [ '#000080', 4 ]
let s:cyan = [ '#008080', 6 ]
let s:green = [ '#008000', 2 ]
if &background ==# 'light'
  let [s:base03, s:base3] = [s:base3, s:base03]
  let [s:base02, s:base2] = [s:base2, s:base02]
  let [s:base01, s:base1] = [s:base1, s:base01]
  let [s:base00, s:base0] = [s:base0, s:base00]
endif
let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:base3, s:blue ], [ s:base3, s:base01 ] ]
let s:p.normal.right = [ [ s:base02, s:base0 ], [ s:base1, s:base01 ] ]
let s:p.inactive.right = [ [ s:base02, s:base01 ], [ s:base00, s:base02 ] ]
let s:p.inactive.left =  [ [ s:base0, s:base02 ], [ s:base00, s:base02 ] ]
let s:p.insert.left = [ [ s:base3, s:green ], [ s:base3, s:base01 ] ]
let s:p.replace.left = [ [ s:base3, s:red ], [ s:base3, s:base01 ] ]
let s:p.visual.left = [ [ s:base3, s:magenta ], [ s:base3, s:base01 ] ]
let s:p.normal.middle = [ [ s:base1, s:base02 ] ]
let s:p.inactive.middle = [ [ s:base0, s:base02 ] ]
let s:p.tabline.left = [ [ s:base2, s:base01 ] ]
let s:p.tabline.tabsel = [ [ s:base2, s:base02 ] ]
let s:p.tabline.middle = [ [ s:base01, s:base2 ] ]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.normal.error = [ [ s:base2, s:red ] ]
let s:p.normal.warning = [ [ s:base02, s:yellow ] ]

let g:lightline#colorscheme#16color#palette = lightline#colorscheme#flatten(s:p)
