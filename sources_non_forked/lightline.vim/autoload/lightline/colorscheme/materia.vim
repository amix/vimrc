" =============================================================================
" Filename: autoload/lightline/colorscheme/materia.vim
" Author: Lokesh Krishna
" License: MIT License
" Last Change: 2017/11/25 11:13:40.
" =============================================================================

" Common colors
let s:fg     = '#d5dbe5'
let s:blue   = '#89ddff'
let s:green  = '#8bd649'
let s:purple = '#82aaff'
let s:red1   = '#ec5f67'
let s:red2   = '#ec5f67'
let s:yellow = '#ffcc00'

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

if lightline#colorscheme#background() ==# 'light'
  " Light variant
  let s:bg     = '#ffffff'
  let s:gray1  = '#2c393f'
  let s:gray2  = '#d5dbe5'
  let s:gray3  = '#707880'

  let s:p.normal.left     = [ [ s:bg, s:green, 'bold' ], [ s:gray1, s:gray3 ] ]
  let s:p.normal.middle   = [ [ s:gray1, s:gray2 ] ]
  let s:p.inactive.left   = [ [ s:bg,  s:gray3 ], [ s:bg, s:gray3 ] ]
  let s:p.inactive.middle = [ [ s:gray3, s:gray2 ] ]
  let s:p.inactive.right  = [ [ s:bg, s:gray3 ], [ s:bg, s:gray3 ] ]
  let s:p.insert.left     = [ [ s:bg, s:blue, 'bold' ], [ s:gray1, s:gray3 ] ]
  let s:p.replace.left    = [ [ s:bg, s:red1, 'bold' ], [ s:gray1, s:gray3 ] ]
  let s:p.visual.left     = [ [ s:bg, s:purple, 'bold' ], [ s:gray1, s:gray3 ] ]
else
  " Dark variant
  let s:bg     = '#263238'
  let s:gray1  = '#37474f'
  let s:gray2  = '#2c393f'
  let s:gray3  = '#37474f'

  let s:p.normal.left     = [ [ s:bg, s:green, 'bold' ], [ s:fg, s:gray3 ] ]
  let s:p.normal.middle   = [ [ s:fg, s:gray2 ] ]
  let s:p.inactive.left   = [ [ s:gray1,  s:bg ], [ s:gray1, s:bg ] ]
  let s:p.inactive.middle = [ [ s:gray1, s:gray2 ] ]
  let s:p.inactive.right  = [ [ s:gray1, s:bg ], [ s:gray1, s:bg ] ]
  let s:p.insert.left     = [ [ s:bg, s:blue, 'bold' ], [ s:fg, s:gray3 ] ]
  let s:p.replace.left    = [ [ s:bg, s:red1, 'bold' ], [ s:fg, s:gray3 ] ]
  let s:p.visual.left     = [ [ s:bg, s:purple, 'bold' ], [ s:fg, s:gray3 ] ]
endif

" Common
let s:p.normal.right   = [ [ s:bg, s:green, 'bold' ], [ s:bg, s:green, 'bold' ] ]
let s:p.normal.error   = [ [ s:red2,   s:bg ] ]
let s:p.normal.warning = [ [ s:yellow, s:bg ] ]
let s:p.insert.right   = [ [ s:bg, s:blue, 'bold' ], [ s:bg, s:blue, 'bold' ] ]
let s:p.replace.right  = [ [ s:bg, s:red1, 'bold' ], [ s:bg, s:red1, 'bold' ] ]
let s:p.visual.right   = [ [ s:bg, s:purple, 'bold' ], [ s:bg, s:purple, 'bold' ] ]
let s:p.tabline.left   = [ [ s:bg, s:gray3 ] ]
let s:p.tabline.tabsel = [ [ s:bg, s:purple, 'bold' ] ]
let s:p.tabline.middle = [ [ s:gray3, s:gray2 ] ]
let s:p.tabline.right  = copy(s:p.normal.right)

let g:lightline#colorscheme#materia#palette = lightline#colorscheme#fill(s:p)
