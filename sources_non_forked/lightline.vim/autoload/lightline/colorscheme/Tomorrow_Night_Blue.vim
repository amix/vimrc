" =============================================================================
" Filename: autoload/lightline/colorscheme/Tomorrow_Night_Blue.vim
" Author: itchyny
" License: MIT License
" Last Change: 2022/03/15 23:57:49.
" =============================================================================

let s:base3 = '#ffffff'
let s:base23 = '#ffffff'
let s:base2 = '#ffffff'
let s:base1 = '#ffffff'
let s:base0 = '#ffffff'
let s:base00 = '#6060df'
let s:base01 = '#6060af'
let s:base02 = '#606087'
let s:base023 = '#202087'
let s:base03 = '#002451'
let s:red = '#ff9da4'
let s:orange = '#ffc58f'
let s:yellow = '#ffeead'
let s:green = '#d1f1a9'
let s:cyan = '#99ffff'
let s:blue = '#bbdaff'
let s:magenta = '#ebbbff'

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
let s:p.tabline.tabsel = [ [ s:base2, s:base03 ] ]
let s:p.tabline.middle = [ [ s:base01, s:base1 ] ]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.normal.error = [ [ s:base023, s:red ] ]
let s:p.normal.warning = [ [ s:base023, s:yellow ] ]

let g:lightline#colorscheme#Tomorrow_Night_Blue#palette = lightline#colorscheme#fill(s:p)
