" MIT License. Copyright (c) 2013-2015 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:filetypes = get(g:, 'airline#extensions#wordcount#filetypes', '\vhelp|markdown|rst|org')

" adapted from http://stackoverflow.com/questions/114431/fast-word-count-function-in-vim
function! s:update()
  if &ft !~ s:filetypes
    unlet! b:airline_wordcount
    return
  endif

  let old_status = v:statusmsg
  let position = getpos(".")
  exe "silent normal! g\<c-g>"
  let stat = v:statusmsg
  call setpos('.', position)
  let v:statusmsg = old_status

  let parts = split(stat)
  if len(parts) > 11
    let cnt = str2nr(split(stat)[11])
    let spc = g:airline_symbols.space
    let b:airline_wordcount = cnt . spc . 'words' . spc . g:airline_right_alt_sep . spc
  else
    unlet! b:airline_wordcount
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

