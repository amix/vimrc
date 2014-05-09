" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

function! airline#extensions#tabline#unique_tail#format(bufnr, buffers)
  let duplicates = {}
  let tails = {}
  let map = {}
  for nr in a:buffers
    let name = bufname(nr)
    if empty(name)
      let map[nr] = '[No Name]'
    else
      let tail = fnamemodify(name, ':t')
      if has_key(tails, tail)
        let duplicates[nr] = nr
      endif
      let tails[tail] = 1
      let map[nr] = airline#extensions#tabline#default#wrap_name(nr, tail)
    endif
  endfor

  for nr in values(duplicates)
    let map[nr] = airline#extensions#tabline#default#wrap_name(nr, fnamemodify(bufname(nr), ':p:.'))
  endfor

  return map[a:bufnr]
endfunction
