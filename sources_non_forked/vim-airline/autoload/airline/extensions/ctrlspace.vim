" MIT License. Copyright (c) 2013-2015 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:spc = g:airline_symbols.space
let s:padding = s:spc . s:spc . s:spc

function! airline#extensions#ctrlspace#statusline(...)
  let b = airline#builder#new({ 'active': 1 })
  call b.add_section('airline_a', s:padding . g:ctrlspace_symbols.cs . s:padding)
  call b.add_section('airline_b', s:padding . ctrlspace#statusline_mode_segment(s:padding))
  call b.split()
  call b.add_section('airline_x', s:spc . ctrlspace#statusline_tab_segment() . s:spc)
  return b.build()
endfunction

function! airline#extensions#ctrlspace#init(ext)
  let g:ctrlspace_statusline_function = 'airline#extensions#ctrlspace#statusline()'
endfunction
