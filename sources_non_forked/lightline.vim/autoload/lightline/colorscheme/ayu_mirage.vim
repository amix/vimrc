" =============================================================================
" Filename: autoload/lightline/colorscheme/ayu_mirage.vim
" Author: impulse
" License: MIT License
" Last Change: 2019/08/11 11:52:20.
" =============================================================================
let s:base0 = [ '#d9d7ce', 244 ]
let s:base1 = [ '#d9d7ce', 247 ]
let s:base2 = [ '#607080', 248 ]
let s:base3 = [ '#d9d7ce', 252 ]
let s:base00 = [ '#272d38', 242  ]
let s:base01 = [ '#272d38', 240 ]
let s:base02 = [ '#212733', 238 ]
let s:base023 = [ '#212733', 236 ]
let s:base03 = [ '#ffc44c', 235 ]
let s:yellow = [ '#ffc44c', 180 ]
let s:orange = [ '#ffae57', 173 ]
let s:red = [ '#f07178', 203 ]
let s:magenta = [ '#d4bfff', 216 ]
let s:blue = [ '#59c2ff', 117 ]
let s:cyan = s:blue
let s:green = [ '#bbe67e', 119 ]
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
let g:lightline#colorscheme#ayu_mirage#palette = lightline#colorscheme#flatten(s:p)
