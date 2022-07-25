" =============================================================================
" Filename: autoload/lightline/colorscheme/ayu_mirage.vim
" Author: impulse
" License: MIT License
" Last Change: 2020/05/01 19:37:21.
" =============================================================================

let s:base0 = '#d9d7ce'
let s:base1 = '#d9d7ce'
let s:base2 = '#607080'
let s:base3 = '#d9d7ce'
let s:base00 = '#272d38'
let s:base01 = '#272d38'
let s:base02 = '#212733'
let s:base023 = '#212733'
let s:base03 = '#ffc44c'
let s:yellow = '#ffc44c'
let s:orange = '#ffae57'
let s:red = '#f07178'
let s:magenta = '#d4bfff'
let s:blue = '#59c2ff'
let s:cyan = s:blue
let s:green = '#bbe67e'

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:base02, s:blue ], [ s:base3, s:base01 ] ]
let s:p.normal.middle = [ [ s:base2, s:base02 ] ]
let s:p.normal.right = [ [ s:base02, s:base0 ], [ s:base1, s:base01 ] ]
let s:p.inactive.left =  [ [ s:base1, s:base01 ], [ s:base3, s:base01 ] ]
let s:p.inactive.middle = [ [ s:base1, s:base023 ] ]
let s:p.inactive.right = [ [ s:base1, s:base01 ], [ s:base2, s:base02 ] ]
let s:p.insert.left = [ [ s:base02, s:green ], [ s:base3, s:base01 ] ]
let s:p.replace.left = [ [ s:base023, s:red ], [ s:base3, s:base01 ] ]
let s:p.visual.left = [ [ s:base02, s:magenta ], [ s:base3, s:base01 ] ]
let s:p.tabline.tabsel = [ [ s:base02, s:base03 ] ]
let s:p.tabline.left = [ [ s:base3, s:base00 ] ]
let s:p.tabline.middle = [ [ s:base2, s:base02 ] ]
let s:p.tabline.right = [ [ s:base2, s:base00 ] ]
let s:p.normal.error = [ [ s:base03, s:red ] ]
let s:p.normal.warning = [ [ s:base023, s:yellow ] ]

let g:lightline#colorscheme#ayu_mirage#palette = lightline#colorscheme#fill(s:p)
