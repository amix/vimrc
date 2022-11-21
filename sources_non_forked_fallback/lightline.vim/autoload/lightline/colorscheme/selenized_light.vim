" =============================================================================
" Filename: autoload/lightline/colorscheme/selenized_light.vim
" Author: itchyny
" License: MIT License
" Last Change: 2020/05/02 16:58:00.
" =============================================================================

" https://github.com/jan-warchol/selenized/blob/master/the-values.md#selenized-light
let s:bg_1      = ['#ece3cc', 0]
let s:bg_2      = ['#d5cdb6', 8]
let s:dim_0     = ['#909995', 7]
let s:red       = ['#d2212d', 1]
let s:green     = ['#489100', 2]
let s:yellow    = ['#ad8900', 3]
let s:blue      = ['#0072d4', 4]
let s:magenta   = ['#ca4898', 5]
let s:cyan      = ['#009c8f', 6]
let s:brred     = ['#cc1729', 9]
let s:brgreen   = ['#428b00', 10]
let s:bryellow  = ['#a78300', 11]
let s:brblue    = ['#006dce', 12]
let s:brmagenta = ['#c44392', 13]
let s:brcyan    = ['#00978a', 14]

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

let g:lightline#colorscheme#selenized_light#palette = lightline#colorscheme#flatten(s:p)
