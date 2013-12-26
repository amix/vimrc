" MIT License. Copyright (c) 2013 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:fmod = get(g:, 'airline#extensions#tabline#fnamemod', ':~:.')
let s:fnamecollapse = get(g:, 'airline#extensions#tabline#fnamecollapse', 1)
let s:buf_nr_format = get(g:, 'airline#extensions#tabline#buffer_nr_format', '%s: ')
let s:buf_nr_show = get(g:, 'airline#extensions#tabline#buffer_nr_show', 0)
let s:buf_modified_symbol = g:airline_symbols.modified

function! airline#extensions#tabline#formatters#default(bufnr, buffers)
  let _ = ''

  let name = bufname(a:bufnr)
  if empty(name)
    let _ .= '[No Name]'
  else
    if s:fnamecollapse
      let _ .= substitute(fnamemodify(name, s:fmod), '\v\w\zs.{-}\ze(\\|/)', '', 'g')
    else
      let _ .= fnamemodify(name, s:fmod)
    endif
  endif

  return s:wrap_name(a:bufnr, _)
endfunction

function! airline#extensions#tabline#formatters#unique_tail(bufnr, buffers)
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
      let map[nr] = s:wrap_name(nr, tail)
    endif
  endfor

  for nr in values(duplicates)
    let map[nr] = s:wrap_name(nr, fnamemodify(bufname(nr), ':p:.'))
  endfor

  return map[a:bufnr]
endfunction

function! s:wrap_name(bufnr, buffer_name)
  let _ = s:buf_nr_show ? printf(s:buf_nr_format, a:bufnr) : ''
  let _ .= a:buffer_name
  if getbufvar(a:bufnr, '&modified') == 1
    let _ .= s:buf_modified_symbol
  endif
  return _
endfunction

