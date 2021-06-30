" =============================================================================
" Filename: autoload/lightline/colorscheme/OldHope.vim
" Author: tomb0y
" License: MIT License
" Last Change: 2017/10/15 06:20:54.
" =============================================================================

let s:yellow = [ '#e5cd52' , 221 ]
let s:blue = [ '#4fb4d8' , 39 ]
let s:red = [ '#f92672' , 161 ]
let s:green = [ '#78bd65' , 41 ]
let s:orange = [ '#ef7c2a' , 202 ]
let s:white = [ '#ffffff' , 15 ]
let s:lightGray = [ '#848794' , 245 ]
let s:gray = [ '#686b78' , 242 ]
let s:darkGray = [ '#45474f' , 238 ]
let s:veryDarkGray = [ '#1c1d21' , 234 ]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

let s:p.normal.left = [ [ s:white, s:blue ], [ s:white, s:gray ] ]
let s:p.insert.left = [ [ s:white, s:green ], [ s:white, s:gray ] ]
let s:p.visual.left = [ [ s:white, s:orange ], [ s:white, s:gray ] ]
let s:p.replace.left = [ [ s:white, s:red ], [ s:white, s:gray ] ]

let s:p.inactive.right = [ [ s:darkGray, s:gray ], [ s:darkGray, s:gray ] ]
let s:p.inactive.left = [ [ s:lightGray, s:darkGray ], [ s:white, s:darkGray ] ]
let s:p.inactive.middle = [ [ s:white, s:darkGray ] ]

let s:p.normal.middle = [ [ s:white, s:darkGray ] ]
let s:p.normal.error = [ [ s:red, s:darkGray ] ]
let s:p.normal.warning = [ [ s:orange, s:darkGray ] ]

let s:p.tabline.left = [ [ s:lightGray, s:darkGray ] ]
let s:p.tabline.tabsel = [ [ s:darkGray, s:yellow ] ]
let s:p.tabline.middle = [ [ s:yellow, s:veryDarkGray ] ]

let s:p.normal.right = copy(s:p.normal.left)
let s:p.insert.right = copy(s:p.insert.left)
let s:p.visual.right = copy(s:p.visual.left)
let s:p.replace.right = copy(s:p.replace.left)
let s:p.tabline.right = copy(s:p.tabline.left)

let g:lightline#colorscheme#OldHope#palette = lightline#colorscheme#flatten(s:p)
