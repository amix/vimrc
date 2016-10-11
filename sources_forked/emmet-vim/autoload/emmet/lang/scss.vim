function! emmet#lang#scss#findTokens(str) abort
  return emmet#lang#css#findTokens(a:str)
endfunction

function! emmet#lang#scss#parseIntoTree(abbr, type) abort
  if a:abbr =~# '>'
    return emmet#lang#html#parseIntoTree(a:abbr, a:type)
  else
    return emmet#lang#css#parseIntoTree(a:abbr, a:type)
  endif
endfunction

function! emmet#lang#scss#toString(settings, current, type, inline, filters, itemno, indent) abort
  let settings = a:settings
  let current = a:current
  let type = a:type
  let inline = a:inline
  let filters = a:filters
  let itemno = a:itemno
  let indent = a:indent
  let str = ''

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
        let tmp .= attr . ': ' . val . ';'
      endif
    endfor
    if len(tmp) > 0
      let str .= " {\n"
      for line in split(tmp, "\n")
        let str .= indent . line . "\n"
      endfor
    else
      let str .= " {\n"
    endif

    let inner = ''
    for child in current.child
      let inner .= emmet#toString(child, type, inline, filters, itemno)
    endfor
    let inner = substitute(inner, "\n", "\n" . escape(indent, '\'), 'g')
    let inner = substitute(inner, "\n" . escape(indent, '\') . '$', '', 'g')
    let str .= indent . inner . "${cursor}\n}\n"
  else
    return emmet#lang#css#toString(settings, current, type, inline, filters, itemno, indent)
  endif
  return str
endfunction

function! emmet#lang#scss#imageSize() abort
  call emmet#lang#css#imageSize()
endfunction

function! emmet#lang#scss#encodeImage() abort
  return emmet#lang#css#encodeImage()
endfunction

function! emmet#lang#scss#parseTag(tag) abort
  return emmet#lang#css#parseTag(a:tag)
endfunction

function! emmet#lang#scss#toggleComment() abort
  call emmet#lang#css#toggleComment()
endfunction

function! emmet#lang#scss#balanceTag(flag) range abort
  if a:flag == -2 || a:flag == 2
    let curpos = [0, line("'<"), col("'<"), 0]
    call setpos('.', curpos)
  else
    let curpos = emmet#util#getcurpos()
  endif
  if a:flag < 0
    let ret = searchpair('}', '', '.\zs{')
  else
    let ret = searchpair('{', '', '}', 'bW')
  endif
  if ret > 0
    let pos1 = emmet#util#getcurpos()[1:2]
    if a:flag < 0
      let pos2 = searchpairpos('{', '', '}')
    else
      let pos2 = searchpairpos('{', '', '}')
    endif
    let block = [pos1, pos2]
    if emmet#util#regionIsValid(block)
      call emmet#util#selectRegion(block)
      return
    endif
  endif
  if a:flag == -2 || a:flag == 2
    silent! exe 'normal! gv'
  else
    call setpos('.', curpos)
  endif
endfunction

function! emmet#lang#scss#moveNextPrevItem(flag) abort
  return emmet#lang#scss#moveNextPrev(a:flag)
endfunction

function! emmet#lang#scss#moveNextPrev(flag) abort
  call emmet#lang#css#moveNextPrev(a:flag)
endfunction

function! emmet#lang#scss#splitJoinTag() abort
  call emmet#lang#css#splitJoinTag()
endfunction

function! emmet#lang#scss#removeTag() abort
  call emmet#lang#css#removeTag()
endfunction
