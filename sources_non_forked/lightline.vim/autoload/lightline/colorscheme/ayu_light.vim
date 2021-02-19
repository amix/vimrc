" =============================================================================
" Filename: autoload/lightline/colorscheme/ayu_light.vim
" Author: christalib
" License: MIT License
" Last Change: 2020/05/01 19:38:21.
" =============================================================================

let s:base0 = '#5C6773'
let s:base1 = '#5C6773'
let s:base2 = '#828C99'
let s:base3 = '#5C6773'
let s:base00 = '#FFFFFF'
let s:base01 = '#FFFFFF'
let s:base02 = '#FAFAFA'
let s:base023 = '#FAFAFA'
let s:base03 = '#E6B673'
let s:yellow = '#E6B673'
let s:orange = '#FF7733'
let s:red = '#f07178'
let s:magenta = '#A37ACC'
let s:blue = '#59c2ff'
let s:cyan = s:blue
let s:green = '#86B300'

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

let g:lightline#colorscheme#ayu_light#palette = lightline#colorscheme#fill(s:p)
