" =============================================================================
" Filename: autoload/whiteline/colorscheme/selenized_white.vim
" Author: itchyny
" License: MIT License
" Last Change: 2020/05/03 19:34:07.
" =============================================================================

" https://github.com/jan-warchol/selenized/blob/master/the-values.md#selenized-white
let s:bg_1      = ['#ebebeb', 0]
let s:bg_2      = ['#cdcdcd', 8]
let s:dim_0     = ['#878787', 7]
let s:red       = ['#d6000c', 1]
let s:green     = ['#1d9700', 2]
let s:yellow    = ['#c49700', 3]
let s:blue      = ['#0064e4', 4]
let s:magenta   = ['#dd0f9d', 5]
let s:cyan      = ['#00ad9c', 6]
let s:brred     = ['#bf0000', 9]
let s:brgreen   = ['#008400', 10]
let s:bryellow  = ['#af8500', 11]
let s:brblue    = ['#0054cf', 12]
let s:brmagenta = ['#c7008b', 13]
let s:brcyan    = ['#009a8a', 14]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

let s:p.normal.right = [[ s:bg_1, s:blue ], [ s:cyan, s:bg_2 ], [ s:dim_0, s:bg_1 ]]
let s:p.normal.left = [[ s:bg_1, s:blue ], [ s:cyan, s:bg_2 ]]
let s:p.normal.middle = [[ s:dim_0, s:bg_1 ]]
let s:p.normal.error = [[ s:bg_1, s:red ]]
let s:p.normal.warning = [[ s:bg_1, s:yellow ]]

let s:p.insert.right = [[ s:bg_1, s:green ], [ s:cyan, s:bg_2 ], [ s:dim_0, s:bg_1 ]]
let s:p.insert.left = [[ s:bg_1, s:green ], [ s:cyan, s:bg_2 ]]

let s:p.visual.right = [[ s:bg_1, s:magenta ], [ s:cyan, s:bg_2 ], [ s:dim_0, s:bg_1 ]]
let s:p.visual.left = [[ s:bg_1, s:magenta ], [ s:cyan, s:bg_2 ]]

let s:p.inactive.left = [[ s:brblue, s:bg_2 ], [ s:cyan, s:bg_2 ]]
let s:p.inactive.right = [[ s:brblue, s:bg_2 ], [ s:cyan, s:bg_2 ]]

let s:p.replace.right = [[ s:bg_1, s:red ], [ s:cyan, s:bg_2 ], [ s:dim_0, s:bg_1 ]]
let s:p.replace.left = [[ s:bg_1, s:red ], [ s:cyan, s:bg_2 ]]

let s:p.tabline.right = [[ s:bg_1, s:red ]]
let s:p.tabline.left = [[ s:cyan, s:bg_2 ]]
let s:p.tabline.tabsel = [[ s:bg_1, s:blue ]]

let g:lightline#colorscheme#selenized_white#palette = lightline#colorscheme#flatten(s:p)
