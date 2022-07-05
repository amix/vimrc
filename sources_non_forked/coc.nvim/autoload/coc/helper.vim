scriptencoding utf-8
" Helper methods for viml

function! coc#helper#get_charactor(line, col) abort
  return strchars(strpart(a:line, 0, a:col - 1))
endfunction

function! coc#helper#last_character(line) abort
  return strcharpart(a:line, strchars(a:line) - 1, 1)
endfunction

function! coc#helper#obj_equal(one, two) abort
  for key in keys(a:one)
    if a:one[key] != a:two[key]
      return 0
    endif
  endfor
  return 1
endfunction

" get change between two lines
function! coc#helper#str_diff(curr, previous, col) abort
  let end = strpart(a:curr, a:col - 1)
  let start = strpart(a:curr, 0, a:col -1)
  let endOffset = 0
  let startOffset = 0
  let currLen = strchars(a:curr)
  let prevLen = strchars(a:previous)
  if len(end)
    let endLen = strchars(end)
    for i in range(min([prevLen, endLen]))
      if strcharpart(end, endLen - 1 - i, 1) ==# strcharpart(a:previous, prevLen -1 -i, 1)
        let endOffset = endOffset + 1
      else
        break
      endif
    endfor
  endif
  let remain = endOffset == 0 ? a:previous : strcharpart(a:previous, 0, prevLen - endOffset)
  if len(remain)
    for i in range(min([strchars(remain), strchars(start)]))
      if strcharpart(remain, i, 1) ==# strcharpart(start, i ,1)
        let startOffset = startOffset + 1
      else
        break
      endif
    endfor
  endif
  return {
      \ 'start': startOffset,
      \ 'end': prevLen - endOffset,
      \ 'text': strcharpart(a:curr, startOffset, currLen - startOffset - endOffset)
      \ }
endfunction

function! coc#helper#str_apply(content, diff) abort
  let totalLen = strchars(a:content)
  let endLen = totalLen - a:diff['end']
  return strcharpart(a:content, 0, a:diff['start']).a:diff['text'].strcharpart(a:content, a:diff['end'], endLen)
endfunction

" insert inserted to line at position, use ... when result is too long
" line should only contains character has strwidth equals 1
function! coc#helper#str_compose(line, position, inserted) abort
  let width = strwidth(a:line)
  let text = a:inserted
  let res = a:line
  let need_truncate = a:position + strwidth(text) + 1 > width
  if need_truncate
    let remain = width - a:position - 3
    if remain < 2
      " use text for full line, use first & end of a:line, ignore position
      let res = strcharpart(a:line, 0, 1)
      let w = strwidth(res)
      for i in range(strchars(text))
        let c = strcharpart(text, i, 1)
        let a = strwidth(c)
        if w + a <= width - 1
          let w = w + a
          let res = res.c
        endif
      endfor
      let res = res.strcharpart(a:line, w)
    else
      let res = strcharpart(a:line, 0, a:position)
      let w = strwidth(res)
      for i in range(strchars(text))
        let c = strcharpart(text, i, 1)
        let a = strwidth(c)
        if w + a <= width - 3
          let w = w + a
          let res = res.c
        endif
      endfor
      let res = res.'..'
      let w = w + 2
      let res = res.strcharpart(a:line, w)
    endif
  else
    let first = strcharpart(a:line, 0, a:position)
    let res = first.text.strcharpart(a:line, a:position + strwidth(text))
  endif
  return res
endfunction

" Return new dict with keys removed
function! coc#helper#dict_omit(dict, keys) abort
  let res = {}
  for key in keys(a:dict)
    if index(a:keys, key) == -1
      let res[key] = a:dict[key]
    endif
  endfor
  return res
endfunction

" Return new dict with keys only
function! coc#helper#dict_pick(dict, keys) abort
  let res = {}
  for key in keys(a:dict)
    if index(a:keys, key) != -1
      let res[key] = a:dict[key]
    endif
  endfor
  return res
endfunction

" support for float values
function! coc#helper#min(first, ...) abort
  let val = a:first
  for i in range(0, len(a:000) - 1)
    if a:000[i] < val
      let val = a:000[i]
    endif
  endfor
  return val
endfunction

" support for float values
function! coc#helper#max(first, ...) abort
  let val = a:first
  for i in range(0, len(a:000) - 1)
    if a:000[i] > val
      let val = a:000[i]
    endif
  endfor
  return val
endfunction
