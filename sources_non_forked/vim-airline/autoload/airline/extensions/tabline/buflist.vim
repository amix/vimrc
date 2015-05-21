" MIT License. Copyright (c) 2013-2015 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:excludes = get(g:, 'airline#extensions#tabline#excludes', [])

function! airline#extensions#tabline#buflist#invalidate()
  unlet! s:current_buffer_list
endfunction

function! airline#extensions#tabline#buflist#list()
  if exists('s:current_buffer_list')
    return s:current_buffer_list
  endif

  let buffers = []
  let cur = bufnr('%')
  for nr in range(1, bufnr('$'))
    if buflisted(nr) && bufexists(nr)
      let toadd = 1
      for ex in s:excludes
        if match(bufname(nr), ex) >= 0
          let toadd = 0
          break
        endif
      endfor
      if getbufvar(nr, 'current_syntax') == 'qf'
        let toadd = 0
      endif
      if toadd
        call add(buffers, nr)
      endif
    endif
  endfor

  let s:current_buffer_list = buffers
  return buffers
endfunction

