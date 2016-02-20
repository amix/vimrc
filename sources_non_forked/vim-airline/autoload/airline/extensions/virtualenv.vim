" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:spc = g:airline_symbols.space

function! airline#extensions#virtualenv#init(ext)
  call a:ext.add_statusline_func('airline#extensions#virtualenv#apply')
endfunction

function! airline#extensions#virtualenv#apply(...)
  if &filetype =~# "python"
    if get(g:, 'virtualenv_loaded', 0)
      let statusline = virtualenv#statusline()
    else
      let statusline = fnamemodify($VIRTUAL_ENV, ':t')
    endif
    if !empty(statusline)
      call airline#extensions#append_to_section('x',
            \ s:spc.g:airline_right_alt_sep.s:spc.statusline)
    endif
  endif
endfunction

function! airline#extensions#virtualenv#update()
  if &filetype =~# "python"
    call airline#extensions#virtualenv#apply()
    call airline#update_statusline()
  endif
endfunction
