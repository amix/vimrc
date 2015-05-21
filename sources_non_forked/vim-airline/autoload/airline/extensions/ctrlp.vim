" MIT License. Copyright (c) 2013-2015 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !get(g:, 'loaded_ctrlp', 0)
  finish
endif

let s:color_template = get(g:, 'airline#extensions#ctrlp#color_template', 'insert')

function! airline#extensions#ctrlp#generate_color_map(dark, light, white)
  return {
        \ 'CtrlPdark'   : a:dark,
        \ 'CtrlPlight'  : a:light,
        \ 'CtrlPwhite'  : a:white,
        \ 'CtrlParrow1' : [ a:light[1] , a:white[1] , a:light[3] , a:white[3] , ''     ] ,
        \ 'CtrlParrow2' : [ a:white[1] , a:light[1] , a:white[3] , a:light[3] , ''     ] ,
        \ 'CtrlParrow3' : [ a:light[1] , a:dark[1]  , a:light[3] , a:dark[3]  , ''     ] ,
        \ }
endfunction

function! airline#extensions#ctrlp#load_theme(palette)
  if exists('a:palette.ctrlp')
    let theme = a:palette.ctrlp
  else
    let s:color_template = has_key(a:palette, s:color_template) ? s:color_template : 'insert'
    let theme = airline#extensions#ctrlp#generate_color_map(
          \ a:palette[s:color_template]['airline_c'],
          \ a:palette[s:color_template]['airline_b'],
          \ a:palette[s:color_template]['airline_a'])
  endif
  for key in keys(theme)
    call airline#highlighter#exec(key, theme[key])
  endfor
endfunction

" Arguments: focus, byfname, regexp, prv, item, nxt, marked
function! airline#extensions#ctrlp#ctrlp_airline(...)
  let b = airline#builder#new({'active': 1})
  if a:2 == 'file'
    call b.add_section_spaced('CtrlPlight', 'by fname')
  endif
  if a:3
    call b.add_section_spaced('CtrlPlight', 'regex')
  endif
  if get(g:, 'airline#extensions#ctrlp#show_adjacent_modes', 1)
    call b.add_section_spaced('CtrlPlight', a:4)
    call b.add_section_spaced('CtrlPwhite', a:5)
    call b.add_section_spaced('CtrlPlight', a:6)
  else
    call b.add_section_spaced('CtrlPwhite', a:5)
  endif
  call b.add_section_spaced('CtrlPdark', a:7)
  call b.split()
  call b.add_section_spaced('CtrlPdark', a:1)
  call b.add_section_spaced('CtrlPdark', a:2)
  call b.add_section_spaced('CtrlPlight', '%{getcwd()}')
  return b.build()
endfunction

" Argument: len
function! airline#extensions#ctrlp#ctrlp_airline_status(...)
  let len = '%#CtrlPdark# '.a:1
  let dir = '%=%<%#CtrlParrow3#'.g:airline_right_sep.'%#CtrlPlight# '.getcwd().' %*'
  return len.dir
endfunction

function! airline#extensions#ctrlp#apply(...)
  " disable statusline overwrite if ctrlp already did it
  return match(&statusline, 'CtrlPwhite') >= 0 ? -1 : 0
endfunction

function! airline#extensions#ctrlp#init(ext)
  let g:ctrlp_status_func = {
        \ 'main': 'airline#extensions#ctrlp#ctrlp_airline',
        \ 'prog': 'airline#extensions#ctrlp#ctrlp_airline_status',
        \ }
  call a:ext.add_statusline_func('airline#extensions#ctrlp#apply')
  call a:ext.add_theme_func('airline#extensions#ctrlp#load_theme')
endfunction

