scriptencoding utf-8

function! coc#dict#equal(one, two) abort
  for key in keys(a:one)
    if a:one[key] != a:two[key]
      return 0
    endif
  endfor
  return 1
endfunction

" Return new dict with keys removed
function! coc#dict#omit(dict, keys) abort
  let res = {}
  for key in keys(a:dict)
    if index(a:keys, key) == -1
      let res[key] = a:dict[key]
    endif
  endfor
  return res
endfunction

" Return new dict with keys only
function! coc#dict#pick(dict, keys) abort
  let res = {}
  for key in keys(a:dict)
    if index(a:keys, key) != -1
      let res[key] = a:dict[key]
    endif
  endfor
  return res
endfunction
