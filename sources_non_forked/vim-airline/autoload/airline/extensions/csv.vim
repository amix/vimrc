" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !get(g:, 'loaded_csv', 0) && !exists(':Table')
  finish
endif

let s:column_display = get(g:, 'airline#extensions#csv#column_display', 'Number')

function! airline#extensions#csv#get_column()
  if exists('*CSV_WCol')
    if s:column_display ==# 'Name'
      return '['.CSV_WCol('Name').CSV_WCol().']'
    else
      return '['.CSV_WCol().']'
    endif
  endif
  return ''
endfunction

function! airline#extensions#csv#apply(...)
  if &ft ==# "csv"
    call airline#extensions#prepend_to_section('gutter',
          \ g:airline_left_alt_sep.' %{airline#extensions#csv#get_column()}')
  endif
endfunction

function! airline#extensions#csv#init(ext)
  call a:ext.add_statusline_func('airline#extensions#csv#apply')
endfunction

