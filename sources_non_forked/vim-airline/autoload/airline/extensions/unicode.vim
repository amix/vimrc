" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !get(g:, 'loaded_unicodePlugin', 0)
  finish
endif

function! airline#extensions#unicode#apply(...)
  if exists(":UnicodeTable") == 2 && bufname('') ==# 'UnicodeTable'
    call airline#parts#define('unicode', {
          \ 'text': '[UnicodeTable]',
          \ 'accent': 'bold' })
    let w:airline_section_a = airline#section#create(['unicode'])
    let w:airline_section_b = ''
    let w:airline_section_c = ''
    let w:airline_section_y = ''
  endif
endfunction

function! airline#extensions#unicode#init(ext)
  call a:ext.add_statusline_func('airline#extensions#unicode#apply')
endfunction
