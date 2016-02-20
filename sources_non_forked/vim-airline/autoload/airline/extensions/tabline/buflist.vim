" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:excludes = get(g:, 'airline#extensions#tabline#excludes', [])
let s:exclude_preview = get(g:, 'airline#extensions#tabline#exclude_preview', 1)

function! airline#extensions#tabline#buflist#invalidate()
  unlet! s:current_buffer_list
endfunction

function! airline#extensions#tabline#buflist#list()
  if exists('s:current_buffer_list')
    return s:current_buffer_list
  endif

  let list = (exists('g:did_bufmru') && g:did_bufmru) ? BufMRUList() : range(1, bufnr("$"))

  let buffers = []
  " If this is too slow, we can switch to a different algorithm.
  " Basically branch 535 already does it, but since it relies on
  " BufAdd autocommand, I'd like to avoid this if possible.
  for nr in list
    if buflisted(nr)
      " Do not add to the bufferlist, if either
      " 1) buffername matches exclude pattern
      " 2) buffer is a quickfix buffer
      " 3) exclude preview windows (if 'bufhidden' == wipe
      "    and 'buftype' == nofile
      if (!empty(s:excludes) && match(bufname(nr), join(s:excludes, '\|')) > -1) ||
            \ (getbufvar(nr, 'current_syntax') == 'qf') ||
            \  (s:exclude_preview && getbufvar(nr, '&bufhidden') == 'wipe'
            \  && getbufvar(nr, '&buftype') == 'nofile')
        continue
      endif
      call add(buffers, nr)
    endif
  endfor

  let s:current_buffer_list = buffers
  return buffers
endfunction

