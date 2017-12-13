" =============================================================================
" Filename: autoload/lightline/colorscheme/material.vim
" Author: Lokesh Krishna
" License: MIT License
" Last Change: 2017/11/25 11:13:42.
" =============================================================================

" Common colors
let s:fg     = '#eeffff'
let s:blue   = '#82aaff'
let s:green  = '#c3e88d'
let s:purple = '#c792ea'
let s:red1   = '#f07178'
let s:red2   = '#ff5370'
let s:yellow = '#ffcb6b'

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

if lightline#colorscheme#background() ==# 'light'
  " Light variant
  let s:bg     = '#ffffff'
  let s:gray1  = '#2e3c43'
  let s:gray2  = '#eeffff'
  let s:gray3  = '#546e7a'

  let s:p.normal.left     = [ [ s:bg, s:blue, 'bold' ], [ s:gray1, s:gray3 ] ]
  let s:p.normal.middle   = [ [ s:gray1, s:gray2 ] ]
  let s:p.inactive.left   = [ [ s:bg,  s:gray3 ], [ s:bg, s:gray3 ] ]
  let s:p.inactive.middle = [ [ s:gray3, s:gray2 ] ]
  let s:p.inactive.right  = [ [ s:bg, s:gray3 ], [ s:bg, s:gray3 ] ]
  let s:p.insert.left     = [ [ s:bg, s:green, 'bold' ], [ s:gray1, s:gray3 ] ]
  let s:p.replace.left    = [ [ s:bg, s:red1, 'bold' ], [ s:gray1, s:gray3 ] ]
  let s:p.visual.left     = [ [ s:bg, s:purple, 'bold' ], [ s:gray1, s:gray3 ] ]
else
  " Dark variant
  let s:bg     = '#263238'
  let s:gray1  = '#314549'
  let s:gray2  = '#2E3C43'
  let s:gray3  = '#314549'

  let s:p.normal.left     = [ [ s:bg, s:blue, 'bold' ], [ s:fg, s:gray3 ] ]
  let s:p.normal.middle   = [ [ s:fg, s:gray2 ] ]
  let s:p.inactive.left   = [ [ s:gray1,  s:bg ], [ s:gray1, s:bg ] ]
  let s:p.inactive.middle = [ [ s:gray1, s:gray2 ] ]
  let s:p.inactive.right  = [ [ s:gray1, s:bg ], [ s:gray1, s:bg ] ]
  let s:p.insert.left     = [ [ s:bg, s:green, 'bold' ], [ s:fg, s:gray3 ] ]
  let s:p.replace.left    = [ [ s:bg, s:red1, 'bold' ], [ s:fg, s:gray3 ] ]
  let s:p.visual.left     = [ [ s:bg, s:purple, 'bold' ], [ s:fg, s:gray3 ] ]
endif

" Common
let s:p.normal.right   = [ [ s:bg, s:blue, 'bold' ], [ s:bg, s:blue, 'bold' ] ]
let s:p.normal.error   = [ [ s:red2,   s:bg ] ]
let s:p.normal.warning = [ [ s:yellow, s:bg ] ]
let s:p.insert.right   = [ [ s:bg, s:green, 'bold' ], [ s:bg, s:green, 'bold' ] ]
let s:p.replace.right  = [ [ s:bg, s:red1, 'bold' ], [ s:bg, s:red1, 'bold' ] ]
let s:p.visual.right   = [ [ s:bg, s:purple, 'bold' ], [ s:bg, s:purple, 'bold' ] ]
let s:p.tabline.left   = [ [ s:fg, s:gray3 ] ]
let s:p.tabline.tabsel = [ [ s:bg, s:purple, 'bold' ] ]
let s:p.tabline.middle = [ [ s:gray3, s:gray2 ] ]
let s:p.tabline.right  = [ [ s:bg, s:red1, 'bold' ] ]

let g:lightline#colorscheme#material#palette = lightline#colorscheme#fill(s:p)
