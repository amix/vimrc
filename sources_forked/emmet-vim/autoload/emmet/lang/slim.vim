function! emmet#lang#slim#findTokens(str) abort
  return emmet#lang#html#findTokens(a:str)
endfunction

function! emmet#lang#slim#parseIntoTree(abbr, type) abort
  return emmet#lang#html#parseIntoTree(a:abbr, a:type)
endfunction

function! emmet#lang#slim#toString(settings, current, type, inline, filters, itemno, indent) abort
  let current = a:current
  let type = a:type
  let inline = a:inline
  let filters = a:filters
  let itemno = a:itemno
  let indent = emmet#getIndentation(type)
  let dollar_expr = emmet#getResource(type, 'dollar_expr', 1)
  let str = ''

  let current_name = current.name
  if dollar_expr
    let current_name = substitute(current.name, '\$$', itemno+1, '')
  endif
  if len(current.name) > 0
    let str .= current_name
    for attr in emmet#util#unique(current.attrs_order + keys(current.attr))
      if !has_key(current.attr, attr)
        continue
      endif
      let Val = current.attr[attr]
      if type(Val) == 2 && Val == function('emmet#types#true')
        let str .= ' ' . attr . '=true'
      else
        if dollar_expr
          while Val =~# '\$\([^#{]\|$\)'
            let Val = substitute(Val, '\(\$\+\)\([^{]\|$\)', '\=printf("%0".len(submatch(1))."d", itemno+1).submatch(2)', 'g')
          endwhile
        endif
        let attr = substitute(attr, '\$$', itemno+1, '')
        let str .= ' ' . attr . '="' . Val . '"'
      endif
    endfor

    let inner = ''
    if len(current.value) > 0
      let str .= "\n"
      let text = current.value[1:-2]
      if dollar_expr
        let text = substitute(text, '\%(\\\)\@\<!\(\$\+\)\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d", itemno+1).submatch(2)', 'g')
        let text = substitute(text, '\${nr}', "\n", 'g')
        let text = substitute(text, '\\\$', '$', 'g')
        let str = substitute(str, '\$#', text, 'g')
      endif
      for line in split(text, "\n")
        let str .= indent . '| ' . line . "\n"
      endfor
    elseif len(current.child) == 0
      let str .= '${cursor}'
    endif
    if len(current.child) == 1 && len(current.child[0].name) == 0
      let str .= "\n"
      let text = current.child[0].value[1:-2]
      if dollar_expr
        let text = substitute(text, '\%(\\\)\@\<!\(\$\+\)\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d", itemno+1).submatch(2)', 'g')
        let text = substitute(text, '\${nr}', "\n", 'g')
        let text = substitute(text, '\\\$', '$', 'g')
      endif
      for line in split(text, "\n")
        let str .= indent . '| ' . line . "\n"
      endfor
    elseif len(current.child) > 0
      for child in current.child
        let inner .= emmet#toString(child, type, inline, filters, itemno, indent)
      endfor
      let inner = substitute(inner, "\n", "\n" . escape(indent, '\'), 'g')
      let inner = substitute(inner, "\n" . escape(indent, '\') . '$', '', 'g')
      let str .= "\n" . indent . inner
    endif
  else
    let str = current.value[1:-2]
    if dollar_expr
      let str = substitute(str, '\%(\\\)\@\<!\(\$\+\)\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d", itemno+1).submatch(2)', 'g')
      let str = substitute(str, '\${nr}', "\n", 'g')
      let str = substitute(str, '\\\$', '$', 'g')
    endif
  endif
  if str !~# "\n$"
    let str .= "\n"
  endif
  return str
endfunction

function! emmet#lang#slim#imageSize() abort
  let line = getline('.')
  let current = emmet#lang#slim#parseTag(line)
  if empty(current) || !has_key(current.attr, 'src')
    return
  endif
  let fn = current.attr.src
  if fn =~# '^\s*$'
    return
  elseif fn !~# '^\(/\|http\)'
    let fn = simplify(expand('%:h') . '/' . fn)
  endif

  let [width, height] = emmet#util#getImageSize(fn)
  if width == -1 && height == -1
    return
  endif
  let current.attr.width = width
  let current.attr.height = height
  let current.attrs_order += ['width', 'height']
  let slim = emmet#toString(current, 'slim', 1)
  let slim = substitute(slim, '\${cursor}', '', '')
  call setline('.', substitute(matchstr(line, '^\s*') . slim, "\n", '', 'g'))
endfunction

function! emmet#lang#slim#encodeImage() abort
endfunction

function! emmet#lang#slim#parseTag(tag) abort
  let current = emmet#newNode()
  let mx = '\([a-zA-Z][a-zA-Z0-9]*\)\s\+\(.*\)'
  let match = matchstr(a:tag, mx)
  let current.name = substitute(match, mx, '\1', '')
  let attrs = substitute(match, mx, '\2', '')
  let mx = '\([a-zA-Z0-9]\+\)=\%(\([^"'' \t]\+\)\|"\([^"]\{-}\)"\|''\([^'']\{-}\)''\)'
  while len(attrs) > 0
    let match = matchstr(attrs, mx)
    if len(match) == 0
      break
    endif
    let attr_match = matchlist(match, mx)
    let name = attr_match[1]
    let value = len(attr_match[2]) ? attr_match[2] : attr_match[3]
    let current.attr[name] = value
    let current.attrs_order += [name]
    let attrs = attrs[stridx(attrs, match) + len(match):]
  endwhile
  return current
endfunction

function! emmet#lang#slim#toggleComment() abort
  let line = getline('.')
  let space = matchstr(line, '^\s*')
  if line =~# '^\s*/'
    call setline('.', space . line[len(space)+1:])
  elseif line =~# '^\s*[a-z]'
    call setline('.', space . '/' . line[len(space):])
  endif
endfunction

function! emmet#lang#slim#balanceTag(flag) range abort
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

function! emmet#lang#slim#moveNextPrevItem(flag) abort
  return emmet#lang#slim#moveNextPrev(a:flag)
endfunction

function! emmet#lang#slim#moveNextPrev(flag) abort
  let pos = search('""\|\(^\s*|\s*\zs\)', a:flag ? 'Wpb' : 'Wp')
  if pos == 2
    startinsert!
  elseif pos != 0
    silent! normal! l
    startinsert
  endif
endfunction

function! emmet#lang#slim#splitJoinTag() abort
  let n = line('.')
  while n > 0
    if getline(n) =~# '^\s*\ze[a-z]'
      let sn = n
      let n += 1
      if getline(n) =~# '^\s*|'
        while n <= line('$')
          if getline(n) !~# '^\s*|'
            break
          endif
          exe n 'delete'
        endwhile
        call setpos('.', [0, sn, 1, 0])
      else
        let spaces = matchstr(getline(sn), '^\s*')
        call append(sn, spaces . '  | ')
        call setpos('.', [0, sn+1, 1, 0])
        startinsert!
      endif
      break
    endif
    let n -= 1
  endwhile
endfunction

function! emmet#lang#slim#removeTag() abort
  let n = line('.')
  let ml = 0
  while n > 0
    if getline(n) =~# '^\s*\ze[a-z]'
      let ml = len(matchstr(getline(n), '^\s*[a-z]'))
      break
    endif
    let n -= 1
  endwhile
  let sn = n
  while n < line('$')
    let l = len(matchstr(getline(n), '^\s*[a-z]'))
    if l > 0 && l <= ml
      let n -= 1
      break
    endif
    let n += 1
  endwhile
  if sn == n
    exe 'delete'
  else
    exe sn ',' (n-1) 'delete'
  endif
endfunction
