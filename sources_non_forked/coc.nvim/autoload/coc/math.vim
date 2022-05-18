
" support for float values
function! coc#math#min(first, ...) abort
  let val = a:first
  for i in range(0, len(a:000) - 1)
    if a:000[i] < val
      let val = a:000[i]
    endif
  endfor
  return val
endfunction
