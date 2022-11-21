" =============================================================================
" Filename: autoload/lightline/colorscheme/16color.vim
" Author: itchyny, jackno
" License: MIT License
" =============================================================================

let s:black = [ '#000000', 0 ]
let s:maroon = [ '#800000', 1 ]
let s:green = [ '#008000', 2 ]
let s:olive = [ '#808000', 3 ]
let s:navy = [ '#000080', 4 ]
let s:purple = [ '#800080', 5 ]
let s:teal = [ '#008080', 6 ]
let s:silver = [ '#c0c0c0', 7 ]
let s:gray = [ '#808080', 8]
let s:red = [ '#ff0000', 9 ]
let s:lime = [ '#00ff00', 10 ]
let s:yellow = [ '#ffff00', 11 ]
let s:blue = [ '#0000ff', 12 ]
let s:fuchsia = [ '#ff00ff', 13 ]
let s:aqua = [ '#00ffff', 14 ]
let s:white = [ '#ffffff', 15 ]

if lightline#colorscheme#background() ==# 'light'
  let [s:black, s:white] = [s:white, s:black]
  let [s:silver, s:gray] = [s:gray, s:silver]
  let [s:blue, s:aqua] = [s:aqua, s:blue]
  let [s:purple, s:fuchsia] = [s:fuchsia, s:purple]
  let [s:green, s:lime] = [s:lime, s:green]
  let [s:red, s:yellow] = [s:yellow, s:red]
endif

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:white, s:blue ], [ s:white, s:gray ] ]
let s:p.normal.middle = [ [ s:silver, s:black ] ]
let s:p.normal.right = [ [ s:white, s:blue ], [ s:white, s:gray ] ]
let s:p.normal.error = [ [ s:black, s:red ] ]
let s:p.normal.warning = [ [ s:black, s:yellow ] ]
let s:p.inactive.left =  [ [ s:silver, s:gray ], [ s:gray, s:black ] ]
let s:p.inactive.middle = [ [ s:silver, s:black ] ]
let s:p.inactive.right = [ [ s:silver, s:gray ], [ s:gray, s:black ] ]
let s:p.insert.left = [ [ s:white, s:green ], [ s:white, s:gray ] ]
let s:p.insert.right = copy(s:p.insert.left)
let s:p.replace.left = [ [ s:white, s:red ], [ s:white, s:gray ] ]
let s:p.replace.right = copy(s:p.replace.left)
let s:p.visual.left = [ [ s:white, s:purple ], [ s:white, s:gray ] ]
let s:p.visual.right = copy(s:p.visual.left)
let s:p.tabline.left = [ [ s:silver, s:black ] ]
let s:p.tabline.tabsel = copy(s:p.normal.right)
let s:p.tabline.middle = [ [ s:silver, s:black ] ]
let s:p.tabline.right = copy(s:p.normal.right)

let g:lightline#colorscheme#16color#palette = lightline#colorscheme#flatten(s:p)
