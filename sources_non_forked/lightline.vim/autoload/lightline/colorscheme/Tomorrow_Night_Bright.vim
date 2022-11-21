" =============================================================================
" Filename: autoload/lightline/colorscheme/Tomorrow_Night_Bright.vim
" Author: itchyny
" License: MIT License
" Last Change: 2022/03/15 23:57:52.
" =============================================================================

let s:base3 = '#eaeaea'
let s:base23 = '#d0d0d0'
let s:base2 = '#c6c6c6'
let s:base1 = '#b2b2b2'
let s:base0 = '#949494'
let s:base00 = '#767676'
let s:base01 = '#606060'
let s:base02 = '#4e4e4e'
let s:base023 = '#262626'
let s:base03 = '#12124c'
let s:red = '#d54e53'
let s:orange = '#e78c45'
let s:yellow = '#e7c547'
let s:green = '#b9ca4a'
let s:cyan = '#70c0b1'
let s:blue = '#7aa6da'
let s:magenta = '#c397d8'

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:base023, s:blue ], [ s:base3, s:base01 ] ]
let s:p.normal.right = [ [ s:base02, s:base1 ], [ s:base2, s:base01 ] ]
let s:p.inactive.right = [ [ s:base02, s:base0 ], [ s:base1, s:base01 ] ]
let s:p.inactive.left =  [ [ s:base02, s:base0 ], [ s:base00, s:base03 ] ]
let s:p.insert.left = [ [ s:base023, s:green ], [ s:base3, s:base01 ] ]
let s:p.replace.left = [ [ s:base023, s:orange ], [ s:base3, s:base01 ] ]
let s:p.visual.left = [ [ s:base023, s:magenta ], [ s:base3, s:base01 ] ]
let s:p.normal.middle = [ [ s:base1, s:base02 ] ]
let s:p.inactive.middle = [ [ s:base0, s:base02 ] ]
let s:p.tabline.left = [ [ s:base2, s:base01 ] ]
let s:p.tabline.tabsel = [ [ s:base2, s:base023 ] ]
let s:p.tabline.middle = [ [ s:base01, s:base1 ] ]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.normal.error = [ [ s:red, s:base023 ] ]
let s:p.normal.warning = [ [ s:yellow, s:base02 ] ]

let g:lightline#colorscheme#Tomorrow_Night_Bright#palette = lightline#colorscheme#fill(s:p)
