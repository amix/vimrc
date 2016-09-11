" =============================================================================
" Filename: autoload/lightline/colorscheme/PaperColor_light.vim
" Author: TKNGUE
" License: MIT License
" Last Change: 2015/07/28 07:46:40.
" =============================================================================

let s:red = '#df0000'
let s:green = '#008700'
let s:blue = '#4271ae'
let s:pink = '#d7005f'
let s:olive = '#718c00'
let s:navy = '#005f87'
let s:orange = '#d75f00'
let s:purple = '#8959a8'
let s:aqua = '#3e999f'

" Basics:
let s:foreground = '#4d4d4c'
let s:background = '#F5F5F5'
let s:window = '#efefef'
let s:status = s:aqua
let s:error = '#ffafdf'

" Tabline:
let s:tabline_bg = s:navy
let s:tabline_active_fg = s:foreground
let s:tabline_active_bg = s:window
let s:tabline_inactive_fg = s:background
let s:tabline_inactive_bg = s:aqua

" Statusline:
let s:statusline_active_fg = s:window
let s:statusline_active_bg = s:navy
let s:statusline_inactive_fg = s:foreground
let s:statusline_inactive_bg = '#dadada'

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:foreground, s:background ], [ s:statusline_active_fg, s:status ], [ s:statusline_active_fg, s:statusline_active_bg ] ]
let s:p.normal.right = [ [ s:foreground, s:background ], [ s:statusline_active_fg, s:status ], [ s:statusline_active_fg, s:statusline_active_bg ] ]
let s:p.normal.middle = [ [ s:statusline_active_fg, s:statusline_active_bg ]]
let s:p.inactive.right = [ [ s:foreground, s:background ], [ s:foreground, s:background ] ]
let s:p.inactive.left = [ [ s:foreground, s:background ], [ s:foreground, s:background ] ]
let s:p.inactive.middle = [ [ s:foreground, s:background ], ]
let s:p.insert.left = [ [ s:blue, s:background ], [ s:statusline_active_fg, s:status ], [ s:statusline_active_fg, s:statusline_active_bg ] ]
let s:p.replace.left = [ [ s:background, s:pink ], [s:statusline_active_fg, s:status ], [ s:statusline_active_fg, s:statusline_active_bg ] ]
let s:p.visual.left = [ [ s:background, s:orange ], [s:statusline_active_fg, s:status ], [ s:statusline_active_fg, s:statusline_active_bg ] ]
let s:p.tabline.left = [ [s:tabline_inactive_fg, s:tabline_inactive_bg ]]
let s:p.tabline.tabsel = [ [s:tabline_active_fg, s:tabline_active_bg ] ]
let s:p.tabline.middle = [ [s:tabline_bg, s:tabline_bg]]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.normal.error = [ [ s:background, s:error ] ]
let s:p.normal.warning = [ [ s:background, s:olive ] ]

let g:lightline#colorscheme#PaperColor_light#palette = lightline#colorscheme#fill(s:p)
