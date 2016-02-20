" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:spc = g:airline_symbols.space
let s:padding = s:spc . s:spc . s:spc

function! airline#extensions#ctrlspace#statusline(...)
  let b = airline#builder#new({ 'active': 1 })
  call b.add_section('airline_b', '⌗' . s:padding . ctrlspace#api#StatuslineModeSegment(s:padding))
  call b.split()
  call b.add_section('airline_x', s:spc . ctrlspace#api#StatuslineTabSegment() . s:spc)
  return b.build()
endfunction

function! airline#extensions#ctrlspace#init(ext)
  let g:CtrlSpaceStatuslineFunction = "airline#extensions#ctrlspace#statusline()"
endfunction
