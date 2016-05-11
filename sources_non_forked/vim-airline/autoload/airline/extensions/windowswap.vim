" vim: et ts=2 sts=2 sw=2

if !exists('g:loaded_windowswap')
  finish
endif

let s:spc = g:airline_symbols.space

if !exists('g:airline#extensions#windowswap#indicator_text')
  let g:airline#extensions#windowswap#indicator_text = 'WS'
endif

function! airline#extensions#windowswap#init(ext)
  call airline#parts#define_function('windowswap', 'airline#extensions#windowswap#get_status')
endfunction

function! airline#extensions#windowswap#get_status()
  " use new tab-aware api if WS is up to date
  let s:mark = exists('*WindowSwap#IsCurrentWindowMarked') ?
    \WindowSwap#IsCurrentWindowMarked() :
    \(WindowSwap#HasMarkedWindow() && WindowSwap#GetMarkedWindowNum() == winnr())
  if s:mark
    return g:airline#extensions#windowswap#indicator_text.s:spc
  endif
  return ''
endfunction

