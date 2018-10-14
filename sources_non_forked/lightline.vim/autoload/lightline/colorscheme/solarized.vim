" =============================================================================
" Filename: autoload/lightline/colorscheme/solarized.vim
" Author: itchyny
" License: MIT License
" Last Change: 2017/11/25 11:13:46.
" =============================================================================

let s:cuicolors = {
      \ 'base03': [ '8', '234', 'DarkGray' ],
      \ 'base02': [ '0', '235', 'Black' ],
      \ 'base01': [ '10', '239', 'LightGreen' ],
      \ 'base00': [ '11', '240', 'LightYellow' ],
      \ 'base0':  [ '12', '244', 'LightBlue' ],
      \ 'base1':  [ '14', '245', 'LightCyan' ],
      \ 'base2': [ '7', '187', 'LightGray' ],
      \ 'base3': [ '15', '230', 'White' ],
      \ 'yellow': [ '3', '136', 'DarkYellow' ],
      \ 'orange': [ '9', '166', 'LightRed' ],
      \ 'red': [ '1', '124', 'DarkRed' ],
      \ 'magenta': [ '5', '125', 'DarkMagenta' ],
      \ 'violet': [ '13', '61', 'LightMagenta' ],
      \ 'blue': [ '4', '33', 'DarkBlue' ],
      \ 'cyan': [ '6', '37', 'DarkCyan' ],
      \ 'green': [ '2', '64', 'DarkGreen' ],
      \ }

" The following condition only applies for the console and is the same
" condition vim-colors-solarized uses to determine which set of colors
" to use.
let s:solarized_termcolors = get(g:, 'solarized_termcolors', 256)
if s:solarized_termcolors != 256 && &t_Co >= 16
  let s:cuiindex = 0
elseif s:solarized_termcolors == 256
  let s:cuiindex = 1
else
  let s:cuiindex = 2
endif

let s:base03 = [ '#002b36', s:cuicolors.base03[s:cuiindex] ]
let s:base02 = [ '#073642', s:cuicolors.base02[s:cuiindex] ]
let s:base01 = [ '#586e75', s:cuicolors.base01[s:cuiindex] ]
let s:base00 = [ '#657b83', s:cuicolors.base00[s:cuiindex] ]
let s:base0 = [ '#839496', s:cuicolors.base0[s:cuiindex] ]
let s:base1 = [ '#93a1a1', s:cuicolors.base1[s:cuiindex] ]
let s:base2 = [ '#eee8d5', s:cuicolors.base2[s:cuiindex] ]
let s:base3 = [ '#fdf6e3', s:cuicolors.base3[s:cuiindex] ]
let s:yellow = [ '#b58900', s:cuicolors.yellow[s:cuiindex] ]
let s:orange = [ '#cb4b16', s:cuicolors.orange[s:cuiindex] ]
let s:red = [ '#dc322f', s:cuicolors.red[s:cuiindex] ]
let s:magenta = [ '#d33682', s:cuicolors.magenta[s:cuiindex] ]
let s:violet = [ '#6c71c4', s:cuicolors.violet[s:cuiindex] ]
let s:blue = [ '#268bd2', s:cuicolors.blue[s:cuiindex] ]
let s:cyan = [ '#2aa198', s:cuicolors.cyan[s:cuiindex] ]
let s:green = [ '#859900', s:cuicolors.green[s:cuiindex] ]

if lightline#colorscheme#background() ==# 'light'
  let [ s:base03, s:base3 ] = [ s:base3, s:base03 ]
  let [ s:base02, s:base2 ] = [ s:base2, s:base02 ]
  let [ s:base01, s:base1 ] = [ s:base1, s:base01 ]
  let [ s:base00, s:base0 ] = [ s:base0, s:base00 ]
endif

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:base03, s:blue ], [ s:base03, s:base00 ] ]
let s:p.normal.right = [ [ s:base03, s:base1 ], [ s:base03, s:base00 ] ]
let s:p.inactive.right = [ [ s:base03, s:base00 ], [ s:base0, s:base02 ] ]
let s:p.inactive.left =  [ [ s:base0, s:base02 ], [ s:base0, s:base02 ] ]
let s:p.insert.left = [ [ s:base03, s:green ], [ s:base03, s:base00 ] ]
let s:p.replace.left = [ [ s:base03, s:red ], [ s:base03, s:base00 ] ]
let s:p.visual.left = [ [ s:base03, s:magenta ], [ s:base03, s:base00 ] ]
let s:p.normal.middle = [ [ s:base1, s:base02 ] ]
let s:p.inactive.middle = [ [ s:base01, s:base02 ] ]
let s:p.tabline.left = [ [ s:base03, s:base00 ] ]
let s:p.tabline.tabsel = [ [ s:base03, s:base1 ] ]
let s:p.tabline.middle = [ [ s:base0, s:base02 ] ]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.normal.error = [ [ s:base03, s:red ] ]
let s:p.normal.warning = [ [ s:base03, s:yellow ] ]

let g:lightline#colorscheme#solarized#palette = lightline#colorscheme#flatten(s:p)
