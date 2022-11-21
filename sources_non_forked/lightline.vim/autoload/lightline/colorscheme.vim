" =============================================================================
" Filename: autoload/lightline/colorscheme.vim
" Author: itchyny
" License: MIT License
" Last Change: 2019/09/07 11:20:37.
" =============================================================================

let s:save_cpo = &cpo
set cpo&vim

let s:cuicolor = {
      \ 'black'          : 16,
      \ 'white'          : 231,
      \
      \ 'darkestgreen'   : 22,
      \ 'darkgreen'      : 28,
      \ 'mediumgreen'    : 70,
      \ 'brightgreen'    : 148,
      \
      \ 'darkestcyan'    : 23,
      \ 'mediumcyan'     : 117,
      \
      \ 'darkestblue'    : 24,
      \ 'darkblue'       : 31,
      \
      \ 'darkestred'     : 52,
      \ 'darkred'        : 88,
      \ 'mediumred'      : 124,
      \ 'brightred'      : 160,
      \ 'brightestred'   : 196,
      \
      \ 'darkestpurple'  : 55,
      \ 'mediumpurple'   : 98,
      \ 'brightpurple'   : 189,
      \
      \ 'brightorange'   : 208,
      \ 'brightestorange': 214,
      \
      \ 'gray0'          : 233,
      \ 'gray1'          : 235,
      \ 'gray2'          : 236,
      \ 'gray3'          : 239,
      \ 'gray4'          : 240,
      \ 'gray5'          : 241,
      \ 'gray6'          : 244,
      \ 'gray7'          : 245,
      \ 'gray8'          : 247,
      \ 'gray9'          : 250,
      \ 'gray10'         : 252,
      \
      \ 'yellow'         : 136,
      \ 'orange'         : 166,
      \ 'red'            : 160,
      \ 'magenta'        : 125,
      \ 'violet'         : 61,
      \ 'blue'           : 33,
      \ 'cyan'           : 37,
      \ 'green'          : 64,
      \ }

let s:guicolor = {
      \ 'black'          : '#000000',
      \ 'white'          : '#ffffff',
      \
      \ 'darkestgreen'   : '#005f00',
      \ 'darkgreen'      : '#008700',
      \ 'mediumgreen'    : '#5faf00',
      \ 'brightgreen'    : '#afdf00',
      \
      \ 'darkestcyan'    : '#005f5f',
      \ 'mediumcyan'     : '#87dfff',
      \
      \ 'darkestblue'    : '#005f87',
      \ 'darkblue'       : '#0087af',
      \
      \ 'darkestred'     : '#5f0000',
      \ 'darkred'        : '#870000',
      \ 'mediumred'      : '#af0000',
      \ 'brightred'      : '#df0000',
      \ 'brightestred'   : '#ff0000',
      \
      \ 'darkestpurple'  : '#5f00af',
      \ 'mediumpurple'   : '#875fdf',
      \ 'brightpurple'   : '#dfdfff',
      \
      \ 'brightorange'   : '#ff8700',
      \ 'brightestorange': '#ffaf00',
      \
      \ 'gray0'          : '#121212',
      \ 'gray1'          : '#262626',
      \ 'gray2'          : '#303030',
      \ 'gray3'          : '#4e4e4e',
      \ 'gray4'          : '#585858',
      \ 'gray5'          : '#606060',
      \ 'gray6'          : '#808080',
      \ 'gray7'          : '#8a8a8a',
      \ 'gray8'          : '#9e9e9e',
      \ 'gray9'          : '#bcbcbc',
      \ 'gray10'         : '#d0d0d0',
      \
      \ 'yellow'         : '#b58900',
      \ 'orange'         : '#cb4b16',
      \ 'red'            : '#dc322f',
      \ 'magenta'        : '#d33682',
      \ 'violet'         : '#6c71c4',
      \ 'blue'           : '#268bd2',
      \ 'cyan'           : '#2aa198',
      \ 'green'          : '#859900',
      \ }

function! s:convert(rgb) abort
  let rgb = map(matchlist(a:rgb, '#\(..\)\(..\)\(..\)')[1:3], '0 + ("0x".v:val)')
  if len(rgb) == 0
    return 0
  endif
  if rgb[0] == 0xc0 && rgb[1] == 0xc0 && rgb[2] == 0xc0
    return 7
  elseif rgb[0] == 0x80 && rgb[1] == 0x80 && rgb[2] == 0x80
    return 8
  elseif (rgb[0] == 0x80 || rgb[0] == 0x00) && (rgb[1] == 0x80 || rgb[1] == 0x00) && (rgb[2] == 0x80 || rgb[2] == 0x00)
    return (rgb[0] / 0x80) + (rgb[1] / 0x80) * 2 + (rgb[1] / 0x80) * 4
  elseif abs(rgb[0]-rgb[1]) < 3 && abs(rgb[1]-rgb[2]) < 3 && abs(rgb[2]-rgb[0]) < 3
    return s:black((rgb[0] + rgb[1] + rgb[2]) / 3)
  else
    return 16 + ((s:nr(rgb[0]) * 6) + s:nr(rgb[1])) * 6 + s:nr(rgb[2])
  endif
endfunction

function! s:black(x) abort
  if a:x < 0x04
    return 16
  elseif a:x > 0xf4
    return 231
  elseif index([0x00, 0x5f, 0x87, 0xaf, 0xdf, 0xff], a:x) >= 0
    let l = a:x / 0x30
    return ((l * 6) + l) * 6 + l + 16
  else
    return 232 + (a:x < 8 ? 0 : a:x < 0x60 ? (a:x-8)/10 : a:x < 0x76 ? (a:x-0x60)/6+9 : (a:x-8)/10)
  endif
endfunction

function! s:nr(x) abort
  return a:x < 0x2f ? 0 : a:x < 0x73 ? 1 : a:x < 0x9b ? 2 : a:x < 0xc7 ? 3 : a:x < 0xef ? 4 : 5
endfunction

function! s:rgb(r, g, b) abort
  return printf('#%02x%02x%02x', a:r, a:g, a:b)
endfunction

function! s:upconvert(nr) abort
  let x = a:nr * 1
  if x < 7
    let [b, rg] = [x / 4, x % 4]
    let [g, r] = [rg / 2, rg % 2]
    return s:rgb(r * 0x80, g * 0x80, b * 0x80)
  elseif x == 7
    return s:rgb(0xc0, 0xc0, 0xc0)
  elseif x == 8
    return s:rgb(0x80, 0x80, 0x80)
  elseif x < 16
    let y = x - 8
    let [b, rg] = [y / 4, y % 4]
    let [g, r] = [rg / 2, rg % 2]
    return s:rgb(r * 0xff, g * 0xff, b * 0xff)
  elseif x < 232
    let y = x - 16
    let [rg, b] = [y / 6, y % 6]
    let [r, g] = [rg / 6, rg % 6]
    let l = [0x00, 0x5f, 0x87, 0xaf, 0xdf, 0xff]
    return s:rgb(l[r], l[g], l[b])
  elseif x < 241
    let k = (x - 232) * 10 + 8
    return s:rgb(k, k, k)
  elseif x < 243
    let k = (x - 241) * 6 + 0x60
    return s:rgb(k, k, k)
  else
    let k = (x - 232) * 10 + 8
    return s:rgb(k, k, k)
  endif
endfunction

function! lightline#colorscheme#fill(p) abort
  for k in values(a:p)
    for l in values(k)
      for m in l
        if len(m) < 4
          if type(m[0]) == 1 && type(m[1]) == 1
            if m[0] =~# '^\d\+$' && m[1] =~# '^\d\+$'
              call insert(m, s:upconvert(m[1]), 0)
              call insert(m, s:upconvert(m[1]), 0)
            else
              call insert(m, get(s:cuicolor, m[0], s:convert(m[0])), 2)
              call insert(m, get(s:cuicolor, m[1], s:convert(m[1])), 3)
              let m[0] = get(s:guicolor, m[0], m[0])
              let m[1] = get(s:guicolor, m[1], m[1])
            endif
          elseif type(m[0]) == 0 && type(m[1]) == 0
              call insert(m, s:upconvert(m[1]), 0)
              call insert(m, s:upconvert(m[1]), 0)
          endif
        endif
      endfor
    endfor
  endfor
  return a:p
endfunction

function! lightline#colorscheme#flatten(p) abort
  for k in values(a:p)
    for l in values(k)
      for m in range(len(l))
        let attr = ''
        if len(l[m]) == 3 && type(l[m][2]) == 1
          let attr = l[m][2]
        endif
        let l[m] = [l[m][0][0], l[m][1][0], l[m][0][1], l[m][1][1]]
        if !empty(attr)
          call add(l[m], attr)
        endif
      endfor
    endfor
  endfor
  return a:p
endfunction

if has('gui_running') || (has('termguicolors') && &termguicolors)
  function! lightline#colorscheme#background() abort
    return &background
  endfunction
else
  " &background is set inappropriately when the colorscheme sets ctermbg of the Normal group
  function! lightline#colorscheme#background() abort
    let bg_color = synIDattr(synIDtrans(hlID('Normal')), 'bg', 'cterm')
    if bg_color !=# ''
      if bg_color < 16
        return &background
      elseif 232 <= bg_color && bg_color < 244
        return 'dark'
      elseif 244 <= bg_color
        return 'light'
      endif
    endif
    let fg_color = synIDattr(synIDtrans(hlID('Normal')), 'fg', 'cterm')
    if fg_color !=# ''
      if fg_color < 7 || 232 <= fg_color && fg_color < 244
        return 'light'
      elseif 8 <= fg_color && fg_color < 16 || 244 <= fg_color
        return 'dark'
      endif
    endif
    return &background
  endfunction
endif

let &cpo = s:save_cpo
unlet s:save_cpo
