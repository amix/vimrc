" =============================================================================
" Filename: autoload/lightline/colorscheme/rosepine_moon.vim
" Author: lsculv (based on work by sheruost)
" License: MIT License
" Last Change: 2022/11/18 11:30:19.
" =============================================================================

" Reference: https://rosepinetheme.com/palette
" Rosé Pine Moon
let s:base = [ '#232136', 233 ]
let s:surface = [ '#2a273f', 234 ]

let s:overlay = [ '#393552', 235 ]
let s:highlight_m = [ '#44415a', 59 ]
let s:muted = [ '#6e6a86', 60 ]
let s:subtle = [ '#908caa', 103 ]

let s:iris = [ '#c4a7e7', 182 ]
let s:pine = [ '#3e8fb0', 30 ]
let s:foam = [ '#9ccfd8', 152 ]
let s:rose = [ '#ea9a97', 217 ]
let s:love = [ '#eb6f92', 204 ]

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

let g:lightline#colorscheme#rosepine_moon#palette = lightline#colorscheme#flatten(s:p)
