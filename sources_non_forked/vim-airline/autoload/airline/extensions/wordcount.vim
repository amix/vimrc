" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:filetypes = get(g:, 'airline#extensions#wordcount#filetypes', '\vhelp|markdown|rst|org|text|asciidoc')
let s:format = get(g:, 'airline#extensions#wordcount#format', '%d words')
let s:formatter = get(g:, 'airline#extensions#wordcount#formatter', 'default')

function! s:update()
  if match(&ft, s:filetypes) > -1
    let l:mode = mode()
    if l:mode ==# 'v' || l:mode ==# 'V' || l:mode ==# 's' || l:mode ==# 'S'
      let b:airline_wordcount = airline#extensions#wordcount#formatters#{s:formatter}#format()
      let b:airline_change_tick = b:changedtick
    else
      if get(b:, 'airline_wordcount_cache', '') is# '' ||
            \ b:airline_wordcount_cache isnot# get(b:, 'airline_wordcount', '') ||
            \ get(b:, 'airline_change_tick', 0) != b:changedtick
        " cache data
        let b:airline_wordcount = airline#extensions#wordcount#formatters#{s:formatter}#format()
        let b:airline_wordcount_cache = b:airline_wordcount
        let b:airline_change_tick = b:changedtick
      endif
    endif
  endif
endfunction

function! airline#extensions#wordcount#apply(...)
  if &ft =~ s:filetypes
    call airline#extensions#prepend_to_section('z', '%{get(b:, "airline_wordcount", "")}')
  endif
endfunction

function! airline#extensions#wordcount#init(ext)
  call a:ext.add_statusline_func('airline#extensions#wordcount#apply')
  autocmd BufReadPost,CursorMoved,CursorMovedI * call s:update()
endfunction
