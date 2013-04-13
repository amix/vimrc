function! zencoding#lang#sass#findTokens(str)
  return zencoding#lang#html#findTokens(a:str)
endfunction

function! zencoding#lang#sass#parseIntoTree(abbr, type)
  if a:abbr =~ '>'
    return zencoding#lang#html#parseIntoTree(a:abbr, a:type)
  else
    return zencoding#lang#css#parseIntoTree(a:abbr, a:type)
  endif
endfunction

function! zencoding#lang#sass#toString(settings, current, type, inline, filters, itemno, indent)
  let settings = a:settings
  let current = a:current
  let type = a:type
  let inline = a:inline
  let filters = a:filters
  let itemno = a:itemno
  let indent = a:indent
  let str = ""

  let current_name = current.name
  let current_name = substitute(current.name, '\$$', itemno+1, '')
  if len(current.name) > 0
    let str .= current_name
    let tmp = ''
    for attr in keys(current.attr)
      let val = current.attr[attr]
      while val =~ '\$\([^#{]\|$\)'
        let val = substitute(val, '\(\$\+\)\([^{]\|$\)', '\=printf("%0".len(submatch(1))."d", itemno+1).submatch(2)', 'g')
      endwhile
      let attr = substitute(attr, '\$$', itemno+1, '')
      if attr == 'id'
        let str .= '#' . val
      elseif attr == 'class'
        let str .= '.' . val
      else
        let tmp .= attr . ': ' . val
      endif
    endfor
    if len(tmp) > 0
      let str .= "\n"
      for line in split(tmp, "\n")
        let str .= indent . line . "\n"
      endfor
    else
      let str .= "\n"
    endif

    let inner = ''
    for child in current.child
      let inner .= zencoding#toString(child, type, inline, filters, itemno)
    endfor
    let inner = substitute(inner, "\n", "\n" . indent, 'g')
    let inner = substitute(inner, "\n" . indent . "$", "", 'g')
    let str .= indent . inner
  else
    let text = zencoding#lang#css#toString(settings, current, type, inline, filters, itemno, indent)
    let text = substitute(text, '\${cursor}', '', 'g')
    let text = substitute(text, '\s*;$', '', '')
    return text
  endif
  return str
endfunction

function! zencoding#lang#sass#imageSize()
endfunction

function! zencoding#lang#sass#encodeImage()
endfunction

function! zencoding#lang#sass#parseTag(tag)
endfunction

function! zencoding#lang#sass#toggleComment()
endfunction

function! zencoding#lang#sass#balanceTag(flag) range
  let block = zencoding#util#getVisualBlock()
  if a:flag == -2 || a:flag == 2
    let curpos = [0, line("'<"), col("'<"), 0]
  else
    let curpos = getpos('.')
  endif
  let n = curpos[1]
  let ml = len(matchstr(getline(n), '^\s*'))

  if a:flag > 0
    if a:flag == 1 || !zencoding#util#regionIsValid(block)
      let n = line('.')
    else
      while n > 0
        let l = len(matchstr(getline(n), '^\s*\ze[a-z]'))
        if l > 0 && l < ml
          let ml = l
          break
        endif
        let n -= 1
      endwhile
    endif
    let sn = n
    if n == 0
      let ml = 0
    endif
    while n < line('$')
      let l = len(matchstr(getline(n), '^\s*[a-z]'))
      if l > 0 && l <= ml
        let n -= 1
        break
      endif
      let n += 1
    endwhile
    call setpos('.', [0, n, 1, 0])
    normal! V
    call setpos('.', [0, sn, 1, 0])
  else
    while n > 0
      let l = len(matchstr(getline(n), '^\s*\ze[a-z]'))
      if l > 0 && l > ml
        let ml = l
        break
      endif
      let n += 1
    endwhile
    let sn = n
    if n == 0
      let ml = 0
    endif
    while n < line('$')
      let l = len(matchstr(getline(n), '^\s*[a-z]'))
      if l > 0 && l <= ml
        let n -= 1
        break
      endif
      let n += 1
    endwhile
    call setpos('.', [0, n, 1, 0])
    normal! V
    call setpos('.', [0, sn, 1, 0])
  endif
endfunction

function! zencoding#lang#sass#moveNextPrev(flag)
  let pos = search('""\|\(^\s*|\s*\zs\)', a:flag ? 'Wpb' : 'Wp')
  if pos == 2
    startinsert!
  elseif pos != 0
    silent! normal! l
    startinsert
  endif
endfunction

function! zencoding#lang#sass#splitJoinTag()
endfunction

function! zencoding#lang#sass#removeTag()
endfunction
