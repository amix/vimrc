" =============================================================================
" Filename: autoload/lightline/colorscheme/selenized_black.vim
" Author: itchyny
" License: MIT License
" Last Change: 2020/05/02 16:56:50.
" =============================================================================

" https://github.com/jan-warchol/selenized/blob/master/the-values.md#selenized-black
let s:bg_1      = ['#252525', 0]
let s:bg_2      = ['#3b3b3b', 8]
let s:dim_0     = ['#777777', 7]
let s:red       = ['#ed4a46', 1]
let s:green     = ['#70b433', 2]
let s:yellow    = ['#dbb32d', 3]
let s:blue      = ['#368aeb', 4]
let s:magenta   = ['#eb6eb7', 5]
let s:cyan      = ['#3fc5b7', 6]
let s:brred     = ['#ff5e56', 9]
let s:brgreen   = ['#83c746', 10]
let s:bryellow  = ['#efc541', 11]
let s:brblue    = ['#4f9cfe', 12]
let s:brmagenta = ['#ff81ca', 13]
let s:brcyan    = ['#56d8c9', 14]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

let s:p.normal.right = [[ s:bg_1, s:blue ], [ s:cyan, s:bg_2 ], [ s:dim_0, s:bg_1 ]]
let s:p.normal.left = [[ s:bg_1, s:blue ], [ s:cyan, s:bg_2 ]]
let s:p.normal.middle = [[ s:bg_1, s:bg_1 ]]
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

let g:lightline#colorscheme#selenized_black#palette = lightline#colorscheme#flatten(s:p)
