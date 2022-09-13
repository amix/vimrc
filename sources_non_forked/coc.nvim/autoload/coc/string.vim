scriptencoding utf-8

function! coc#string#get_character(line, col) abort
  return strchars(strpart(a:line, 0, a:col - 1))
endfunction

function! coc#string#last_character(line) abort
  return strcharpart(a:line, strchars(a:line) - 1, 1)
endfunction

function! coc#string#reflow(lines, width) abort
  let lines = []
  let currlen = 0
  let parts = []
  for line in a:lines
    for part in split(line, '\s\+')
      let w = strwidth(part)
      if currlen + w + 1 >= a:width
        if len(parts) > 0
          call add(lines, join(parts, ' '))
        endif
        if w >= a:width
          call add(lines, part)
          let currlen = 0
          let parts = []
        else
          let currlen = w
          let parts = [part]
        endif
        continue
      endif
      call add(parts, part)
      let currlen = currlen + w + 1
    endfor
  endfor
  if len(parts) > 0
    call add(lines, join(parts, ' '))
  endif
  return empty(lines) ? [''] : lines
endfunction

" Used when 'wrap' and 'linebreak' is enabled
function! coc#string#content_height(lines, width) abort
  let len = 0
  let pattern = empty(&breakat) ? '.\zs' : '['.substitute(&breakat, '\([\[\]]\)', '\\\1', 'g').']\zs'
  for line in a:lines
    if strwidth(line) <= a:width
      let len += 1
    else
      let currlen = 0
      for part in split(line, pattern)
        let wl = strwidth(part)
        if currlen == 0 && wl > 0
          let len += 1
        endif
        let delta = currlen + wl - a:width
        if delta >= 0
          let len = len + (delta > 0)
          let currlen = delta == 0 ? 0 : wl
          if wl >= a:width
            let currlen = wl%a:width
            let len += float2nr(ceil(wl/(a:width + 0.0))) - (currlen == 0)
          endif
        else
          let currlen = currlen + wl
        endif
      endfor
    endif
  endfor
  return len
endfunction

" get change between two lines
function! coc#string#diff(curr, previous, col) abort
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

function! coc#string#apply(content, diff) abort
  let totalLen = strchars(a:content)
  let endLen = totalLen - a:diff['end']
  return strcharpart(a:content, 0, a:diff['start']).a:diff['text'].strcharpart(a:content, a:diff['end'], endLen)
endfunction

" insert inserted to line at position, use ... when result is too long
" line should only contains character has strwidth equals 1
function! coc#string#compose(line, position, inserted) abort
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
          let res = res . c
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
          let res = res . c
        endif
      endfor
      let res = res.'..'
      let w = w + 2
      let res = res . strcharpart(a:line, w)
    endif
  else
    let first = strcharpart(a:line, 0, a:position)
    let res = first . text . strcharpart(a:line, a:position + strwidth(text))
  endif
  return res
endfunction
