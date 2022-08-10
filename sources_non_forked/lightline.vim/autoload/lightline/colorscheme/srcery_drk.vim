" =============================================================================
" Filename: autoload/lightline/colorscheme/srcery_drk.vim
" Author: Christopher Vittal
" License: MIT License
" Last Change: 2018/05/19
" =============================================================================

let s:base03 = [ '#151513', 233 ]
let s:base02 = [ '#30302c', 236 ]
let s:base01 = [ '#4e4e43', 239 ]
let s:base00 = [ '#666656', 242  ]
let s:base0 = [ '#808070', 244 ]
let s:base1 = [ '#949484', 246 ]
let s:base2 = [ '#a8a897', 248 ]
let s:base3 = [ '#e8e8d3', 253 ]
let s:yellow = [ '#fbb829', 3 ]
let s:orange = [ '#d75f00', 166 ]
let s:red = [ '#ff3128', 1 ]
let s:magenta = [ '#e02c6d', 5 ]
let s:bright_magenta = [ '#e35682', 13 ]
let s:blue = [ '#5573a3', 4 ]
let s:bright_blue = [ '#8eb2f7', 12 ]
let s:cyan = [ '#0aaeb3', 6 ]
let s:green = [ '#519f50', 2 ]
let s:bright_green = [ '#98bc37', 10 ]
let s:white = [ '#fce8c3', 15 ]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:base02, s:bright_blue, 'bold' ], [ s:base3, s:base01 ] ]
let s:p.normal.right = [ [ s:base02, s:base1 ], [ s:base2, s:base01 ] ]
let s:p.inactive.right = [ [ s:base02, s:base00 ], [ s:base0, s:base02 ] ]
let s:p.inactive.left =  [ [ s:base0, s:base02 ], [ s:base00, s:base02 ] ]
let s:p.insert.left = [ [ s:base02, s:bright_green, 'bold' ], [ s:base3, s:base01 ] ]
let s:p.replace.left = [ [ s:base3, s:red, 'bold' ], [ s:base3, s:base01 ] ]
let s:p.visual.left = [ [ s:base3, s:bright_magenta, 'bold' ], [ s:base3, s:base01 ] ]
let s:p.normal.middle = [ [ s:base0, s:base02 ] ]
let s:p.inactive.middle = [ [ s:base00, s:base02 ] ]
let s:p.tabline.left = [ [ s:base3, s:base00, 'bold'] ]
let s:p.tabline.tabsel = [ [ s:base3, s:base02 ] ]
let s:p.tabline.middle = [ [ s:base01, s:base1 ] ]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.normal.error = [ [ s:red, s:base02 ] ]
let s:p.normal.warning = [ [ s:yellow, s:base01 ] ]

let g:lightline#colorscheme#srcery_drk#palette = lightline#colorscheme#flatten(s:p)
