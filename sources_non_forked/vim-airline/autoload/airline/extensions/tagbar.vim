" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !exists(':TagbarToggle')
  finish
endif

let s:flags = get(g:, 'airline#extensions#tagbar#flags', '')
let s:spc = g:airline_symbols.space

" Arguments: current, sort, fname
function! airline#extensions#tagbar#get_status(...)
  let builder = airline#builder#new({ 'active': a:1 })
  call builder.add_section('airline_a', s:spc.'Tagbar'.s:spc)
  call builder.add_section('airline_b', s:spc.a:2.s:spc)
  call builder.add_section('airline_c', s:spc.a:3.s:spc)
  return builder.build()
endfunction

function! airline#extensions#tagbar#inactive_apply(...)
  if getwinvar(a:2.winnr, '&filetype') == 'tagbar'
    return -1
  endif
endfunction

function! airline#extensions#tagbar#currenttag()
  if get(w:, 'airline_active', 0)
    return tagbar#currenttag('%s', '', s:flags)
  endif
  return ''
endfunction

function! airline#extensions#tagbar#init(ext)
  call a:ext.add_inactive_statusline_func('airline#extensions#tagbar#inactive_apply')
  let g:tagbar_status_func = 'airline#extensions#tagbar#get_status'

  call airline#parts#define_function('tagbar', 'airline#extensions#tagbar#currenttag')
endfunction

