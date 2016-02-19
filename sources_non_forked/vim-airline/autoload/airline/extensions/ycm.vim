" MIT License. Copyright (c) 2015 Evgeny Firsov.
" vim: et ts=2 sts=2 sw=2

let s:spc = g:airline_symbols.space
let s:error_symbol = get(g:, 'airline#extensions#ycm#error_symbol', 'E:')
let s:warning_symbol = get(g:, 'airline#extensions#ycm#warning_symbol', 'W:')

function! airline#extensions#ycm#init(ext)
  call airline#parts#define_function('ycm_error_count', 'airline#extensions#ycm#get_error_count')
  call airline#parts#define_function('ycm_warning_count', 'airline#extensions#ycm#get_warning_count')
endfunction

function! airline#extensions#ycm#get_error_count()
  if exists(':YcmDiag')
    let cnt = youcompleteme#GetErrorCount()

    if cnt != 0
      return s:error_symbol.cnt
    endif
  endif

  return ''
endfunction

function! airline#extensions#ycm#get_warning_count()
  if exists(':YcmDiag')
    let cnt = youcompleteme#GetWarningCount()

    if cnt != 0
      return s:warning_symbol.cnt.s:spc
    endif
  endif

  return ''
endfunction

