" MIT License. Copyright (c) 2014 Mathias Andersson.
" vim: et ts=2 sts=2 sw=2
if !exists('*CapsLockStatusline')
  finish
endif

function! airline#extensions#capslock#status()
  return tolower(CapsLockStatusline()) == '[caps]' ? 'CAPS' : ''
endfunction

function! airline#extensions#capslock#init(ext)
  call airline#parts#define_function('capslock', 'airline#extensions#capslock#status')
endfunction

