function! emmet#lang#sass#findTokens(str) abort
  return emmet#lang#css#findTokens(a:str)
endfunction

function! emmet#lang#sass#parseIntoTree(abbr, type) abort
    return emmet#lang#html#parseIntoTree(a:abbr, a:type)
endfunction

function! emmet#lang#sass#toString(settings, current, type, inline, filters, itemno, indent) abort
  let settings = a:settings
  let current = a:current
  let type = a:type
  let inline = a:inline
  let filters = a:filters
  let itemno = a:itemno
  let indent = a:indent
  let str = ''

  let current_name = current.name
  let current_name = substitute(current.name, '\$$', itemno+1, '')
  if len(current.name) > 0
    let str .= current_name
    let tmp = ''
    for attr in keys(current.attr)
      let val = current.attr[attr]
      while val =~# '\$\([^#{]\|$\)'
        let val = substitute(val, '\(\$\+\)\([^{]\|$\)', '\=printf("%0".len(submatch(1))."d", itemno+1).submatch(2)', 'g')
      endwhile
      let attr = substitute(attr, '\$$', itemno+1, '')
      if attr ==# 'id'
        let str .= '#' . val
      elseif attr ==# 'class'
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
      let tmp = emmet#toString(child, type, inline, filters, itemno, indent)
      let tmp = substitute(tmp, "\n", "\n" . escape(indent, '\'), 'g')
      let tmp = substitute(tmp, "\n" . escape(indent, '\') . '$', '${cursor}\n', 'g')
      let inner .= tmp
    endfor
    if len(inner) > 0
      let str .= indent . inner
    endif
  else
    let text = emmet#lang#css#toString(settings, current, type, inline, filters, itemno, indent)
    let text = substitute(text, '\s*;\ze\(\${[^}]\+}\)\?\(\n\|$\)', '', 'g')
    return text
  endif
  return str
endfunction

function! emmet#lang#sass#imageSize() abort
endfunction

function! emmet#lang#sass#encodeImage() abort
endfunction

function! emmet#lang#sass#parseTag(tag) abort
endfunction

function! emmet#lang#sass#toggleComment() abort
endfunction

function! emmet#lang#sass#balanceTag(flag) range abort
  let block = emmet#util#getVisualBlock()
  if a:flag == -2 || a:flag == 2
    let curpos = [0, line("'<"), col("'<"), 0]
  else
    let curpos = emmet#util#getcurpos()
  endif
  let n = curpos[1]
  let ml = len(matchstr(getline(n), '^\s*'))

  if a:flag > 0
    if a:flag == 1 || !emmet#util#regionIsValid(block)
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

function! emmet#lang#sass#moveNextPrevItem(flag) abort
  return emmet#lang#sass#moveNextPrev(a:flag)
endfunction

function! emmet#lang#sass#moveNextPrev(flag) abort
  let pos = search('""\|\(^\s*|\s*\zs\)', a:flag ? 'Wpb' : 'Wp')
  if pos == 2
    startinsert!
  elseif pos != 0
    silent! normal! l
    startinsert
  endif
endfunction

function! emmet#lang#sass#splitJoinTag() abort
endfunction

function! emmet#lang#sass#removeTag() abort
endfunction
