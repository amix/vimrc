" =============================================================================
" Filename: autoload/lightline/colorscheme/powerlineish.vim
" Author: itchyny
" License: MIT License
" Last Change: 2019/06/12 18:47:00.
" =============================================================================

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ ['darkestgreen', 'brightgreen', 'bold'], ['white', 'gray0'] ]
let s:p.normal.right = [ ['gray10', 'gray2'], ['white', 'gray1'], ['white', 'gray0'] ]
let s:p.inactive.right = [ ['gray1', 'gray5'], ['gray4', 'gray1'], ['gray4', 'gray0'] ]
let s:p.inactive.left = s:p.inactive.right[1:]
let s:p.insert.left = [ ['darkestcyan', 'white', 'bold'], ['mediumcyan', 'darkestblue'] ]
let s:p.insert.right = [ [ 'darkestblue', 'mediumcyan' ], [ 'mediumcyan', 'darkblue' ], [ 'mediumcyan', 'darkestblue' ] ]
let s:p.replace.left = [ ['white', 'brightred', 'bold'], ['white', 'gray0'] ]
let s:p.visual.left = [ ['black', 'brightestorange', 'bold'], ['white', 'gray0'] ]
let s:p.normal.middle = [ [ 'white', 'gray0' ] ]
let s:p.insert.middle = [ [ 'mediumcyan', 'darkestblue' ] ]
let s:p.replace.middle = s:p.normal.middle
let s:p.replace.right = s:p.normal.right
let s:p.tabline.left = [ [ 'gray9', 'gray0' ] ]
let s:p.tabline.tabsel = [ [ 'gray9', 'gray2' ] ]
let s:p.tabline.middle = [ [ 'gray2', 'gray0' ] ]
let s:p.tabline.right = [ [ 'gray9', 'gray1' ] ]
let s:p.normal.error = [ [ 'gray9', 'brightestred' ] ]
let s:p.normal.warning = [ [ 'gray1', 'yellow' ] ]

let g:lightline#colorscheme#powerlineish#palette = lightline#colorscheme#fill(s:p)
