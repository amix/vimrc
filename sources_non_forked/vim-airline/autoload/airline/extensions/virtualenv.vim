" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !get(g:, 'virtualenv_loaded', 0)
  finish
endif

let s:spc = g:airline_symbols.space

function! airline#extensions#virtualenv#init(ext)
  call a:ext.add_statusline_func('airline#extensions#virtualenv#apply')
endfunction

function! airline#extensions#virtualenv#apply(...)
  if &filetype =~ "python"
    call airline#extensions#append_to_section('x',
          \ s:spc.g:airline_right_alt_sep.s:spc.'%{virtualenv#statusline()}')
  endif
endfunction

