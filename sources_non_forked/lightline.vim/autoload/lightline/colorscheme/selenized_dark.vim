" =============================================================================
" Filename: autoload/lightline/colorscheme/selenized_dark.vim
" Author: Charles Hall
" License: MIT License
" Last Change: 2019/07/22 11:05:34.
" =============================================================================

" https://github.com/jan-warchol/selenized/blob/master/the-values.md#selenized-dark
let s:black      = [ "#184956", 0  ]
let s:red        = [ "#fa5750", 1  ]
let s:green      = [ "#75b938", 2  ]
let s:yellow     = [ "#dbb32d", 3  ]
let s:blue       = [ "#4695f7", 4  ]
let s:magenta    = [ "#f275be", 5  ]
let s:cyan       = [ "#41c7b9", 6  ]
let s:white      = [ "#72898f", 7  ]
let s:brblack    = [ "#2d5b69", 8  ]
let s:brred      = [ "#ff665c", 9  ]
let s:brgreen    = [ "#84c747", 10 ]
let s:bryellow   = [ "#ebc13d", 11 ]
let s:brblue     = [ "#58a3ff", 12 ]
let s:brmagenta  = [ "#ff84cd", 13 ]
let s:brcyan     = [ "#53d6c7", 14 ]
let s:brwhite    = [ "#cad8d9", 15 ]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

let s:p.normal.right    = [[ s:black,  s:blue    ],[ s:cyan, s:brblack ],[ s:white, s:black ]]
let s:p.normal.left     = [[ s:black,  s:blue    ],[ s:cyan, s:brblack ]]
let s:p.normal.middle   = [[ s:black,  s:black   ]]
let s:p.normal.error    = [[ s:black,  s:red     ]]
let s:p.normal.warning  = [[ s:black,  s:yellow  ]]

let s:p.insert.right    = [[ s:black,  s:green   ],[ s:cyan, s:brblack ],[ s:white, s:black ]]
let s:p.insert.left     = [[ s:black,  s:green   ],[ s:cyan, s:brblack ]]

let s:p.visual.right    = [[ s:black,  s:magenta ],[ s:cyan, s:brblack ],[ s:white, s:black ]]
let s:p.visual.left     = [[ s:black,  s:magenta ],[ s:cyan, s:brblack ]]

let s:p.inactive.left   = [[ s:brblue, s:brblack ],[ s:cyan, s:brblack ]]
let s:p.inactive.right  = [[ s:brblue, s:brblack ],[ s:cyan, s:brblack ]]

let s:p.replace.right   = [[ s:black,  s:red     ],[ s:cyan, s:brblack ],[ s:white, s:black ]]
let s:p.replace.left    = [[ s:black,  s:red     ],[ s:cyan, s:brblack ]]

let s:p.tabline.right   = [[ s:black,  s:red ]]
let s:p.tabline.left    = [[ s:cyan,   s:brblack ]]
let s:p.tabline.tabsel  = [[ s:black,  s:blue ]]

let g:lightline#colorscheme#selenized_dark#palette = lightline#colorscheme#flatten(s:p)
