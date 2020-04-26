" =============================================================================
" Filename: autoload/lightline/colorscheme/ayu_light.vim
" Author: christalib
" License: MIT License
" Last Change: 2020/02/15 18:45:57.
" =============================================================================
let s:base0 = [ '#5C6773', 244 ]
let s:base1 = [ '#5C6773', 247 ]
let s:base2 = [ '#828C99', 248 ]
let s:base3 = [ '#5C6773', 252 ]
let s:base00 = [ '#FFFFFF', 242  ]
let s:base01 = [ '#FFFFFF', 240 ]
let s:base02 = [ '#FAFAFA', 238 ]
let s:base023 = [ '#FAFAFA', 236 ]
let s:base03 = [ '#E6B673', 235 ]
let s:yellow = [ '#E6B673', 180 ]
let s:orange = [ '#FF7733', 173 ]
let s:red = [ '#f07178', 203 ]
let s:magenta = [ '#A37ACC', 216 ]
let s:blue = [ '#59c2ff', 117 ]
let s:cyan = s:blue
let s:green = [ '#86B300', 119 ]
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
let g:lightline#colorscheme#ayu_light#palette = lightline#colorscheme#flatten(s:p)
