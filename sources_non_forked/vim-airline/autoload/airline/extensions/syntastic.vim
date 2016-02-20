" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !exists(':SyntasticCheck')
  finish
endif

function! airline#extensions#syntastic#get_warnings()
  let errors = SyntasticStatuslineFlag()
  if strlen(errors) > 0
    return errors.(g:airline_symbols.space)
  endif
  return ''
endfunction

function! airline#extensions#syntastic#init(ext)
  call airline#parts#define_function('syntastic', 'airline#extensions#syntastic#get_warnings')
endfunction

