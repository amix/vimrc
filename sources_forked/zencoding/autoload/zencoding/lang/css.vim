function! zencoding#lang#css#findTokens(str)
  return substitute(a:str, '^.*[;{]\s*', '', '')
endfunction

function! zencoding#lang#css#parseIntoTree(abbr, type)
  let abbr = a:abbr
  let type = a:type
  let prefix = 0
  let value = ''

  let settings = zencoding#getSettings()
  let indent = zencoding#getIndentation(type)
  
  let root = { 'name': '', 'attr': {}, 'child': [], 'snippet': '', 'multiplier': 1, 'parent': {}, 'value': '', 'pos': 0, 'important': 0 }

  " emmet
  let tokens = split(abbr, '+\ze[^)!]')
  for n in range(len(tokens))
    let token = tokens[n]
    let prop = matchlist(token, '^\(-\{0,1}[a-zA-Z]\+\|[a-zA-Z0-9]\++\{0,1}\|([a-zA-Z0-9]\++\{0,1})\)\(\%([0-9.-]\+[pe]\{0,1}-\{0,1}\|-auto\)*\)$')
    if len(prop)
      let token = substitute(prop[1], '^(\(.*\))', '\1', '')
      if token =~ '^-'
        let prefix = 1
        let token = token[1:]
      endif
      let value = ''
      for v in split(prop[2], '\d\zs-')
        if len(value) > 0
          let value .= ' '
        endif
        if token =~ '^[z]'
          " TODO
          let value .= substitute(v, '[^0-9.]*$', '', '')
        elseif v =~ 'p$'
          let value .= substitute(v, 'p$', '%', '')
        elseif v =~ 'e$'
          let value .= substitute(v, 'e$', 'em', '')
        elseif v =~ '\.'
          let value .= v . 'em'
        elseif v == 'auto'
          let value .= v
        else
          let value .= v . 'px'
        endif
      endfor
    endif
  
    let tag_name = token
    if tag_name =~ '.!$'
      let tag_name = tag_name[:-2]
      let important = 1
    else
      let important = 0
    endif
    " make default node
    let current = { 'name': '', 'attr': {}, 'child': [], 'snippet': '', 'multiplier': 1, 'parent': {}, 'value': '', 'pos': 0, 'important': important }
    let current.name = tag_name
  
    " aliases
    let aliases = zencoding#getResource(type, 'aliases', {})
    if has_key(aliases, tag_name)
      let current.name = aliases[tag_name]
    endif
    let use_pipe_for_cursor = zencoding#getResource(type, 'use_pipe_for_cursor', 1)
  
    " snippets
    let snippets = zencoding#getResource(type, 'snippets', {})
    if !empty(snippets) && has_key(snippets, tag_name)
      let snippet = snippets[tag_name]
      if use_pipe_for_cursor
        let snippet = substitute(snippet, '|', '${cursor}', 'g')
      endif
      let lines = split(snippet, "\n")
      call map(lines, 'substitute(v:val, "\\(    \\|\\t\\)", escape(indent, "\\\\"), "g")')
      let current.snippet = join(lines, "\n")
      let current.name = ''
      let current.snippet = substitute(current.snippet, ';', value . ';', '')
      if use_pipe_for_cursor && len(value) > 0 && stridx(value, '${cursor}') == -1
        let current.snippet = substitute(current.snippet, '${cursor}', '', 'g') . '${cursor}'
      endif
      if n < len(tokens) - 1
        let current.snippet .= "\n"
      endif
    endif
  
    let current.pos = 0
    let lg = matchlist(token, '^\%(linear-gradient\|lg\)(\s*\(\w\+\)\s*,\s*\([^,]\+\)\s*,\s*\([^)]\+\)\s*)$')
    if len(lg)
      let current.name = ''
      let current.snippet = printf("background-image: -webkit-gradient(%s, 0 0, 0 100%, from(%s), to(%s));\n", lg[1], lg[2], lg[3])
      call add(root.child, deepcopy(current))
      let current.snippet = printf("background-image: -webkit-linear-gradient(%s, %s);\n", lg[2], lg[3])
      call add(root.child, deepcopy(current))
      let current.snippet = printf("background-image: -moz-linear-gradient(%s, %s);\n", lg[2], lg[3])
      call add(root.child, deepcopy(current))
      let current.snippet = printf("background-image: -o-linear-gradient(%s, %s);\n", lg[2], lg[3])
      call add(root.child, deepcopy(current))
      let current.snippet = printf("background-image: linear-gradient(%s, %s);\n", lg[2], lg[3])
      call add(root.child, deepcopy(current))
    elseif prefix
      let snippet = current.snippet
      let current.snippet = '-webkit-' . snippet . "\n"
      call add(root.child, deepcopy(current))
      let current.snippet = '-moz-' . snippet . "\n"
      call add(root.child, deepcopy(current))
      let current.snippet = snippet
      call add(root.child, current)
    else
      call add(root.child, current)
    endif
  endfor
  return root
endfunction

function! zencoding#lang#css#toString(settings, current, type, inline, filters, itemno, indent)
  let current = a:current
  let value = current.value[1:-2]
  if zencoding#useFilter(a:filters, 'fc')
    let value = substitute(value, '\([^:]\+\):\([^;]*;\)', '\1: \2', 'g')
  else
    let value = substitute(value, '\([^:]\+\):\([^;]*;\)', '\1:\2', 'g')
  endif
  if current.important
    let value = substitute(value, ';', ' !important;', '')
  endif
  return value
endfunction

function! zencoding#lang#css#imageSize()
endfunction

function! zencoding#lang#css#encodeImage()
endfunction

function! zencoding#lang#css#parseTag(tag)
  return {}
endfunction

function! zencoding#lang#css#toggleComment()
  let line = getline('.')
  let mx = '^\(\s*\)/\*\s*\(.*\)\s*\*/\s*$'
  if line =~ '{\s*$'
    let block = zencoding#util#searchRegion('/\*', '\*/\zs')
    if zencoding#util#regionIsValid(block)
      let content = zencoding#util#getContent(block)
      let content = substitute(content, '/\*\s\(.*\)\s\*/', '\1', '')
      call zencoding#util#setContent(block, content)
    else
      let node = expand('<cword>')
      if len(node)
        exe "normal ciw\<c-r>='/* '.node.' */'\<cr>"
      endif
    endif
  else
    if line =~ mx
      let space = substitute(matchstr(line, mx), mx, '\1', '')
      let line = substitute(matchstr(line, mx), mx, '\2', '')
      let line = space . substitute(line, '^\s*\|\s*$', '\1', 'g')
    else
      let mx = '^\(\s*\)\(.*\)\s*$'
      let line = substitute(line, mx, '\1/* \2 */', '')
    endif
    call setline('.', line)
  endif
endfunction

function! zencoding#lang#css#balanceTag(flag) range
  if a:flag == -2 || a:flag == 2
    let curpos = [0, line("'<"), col("'<"), 0]
  else
    let curpos = getpos('.')
  endif
  let block = zencoding#util#getVisualBlock()
  if !zencoding#util#regionIsValid(block)
    if a:flag > 0
      let block = zencoding#util#searchRegion('^', ';')
      if zencoding#util#regionIsValid(block)
        call zencoding#util#selectRegion(block)
        return
      endif
    endif
  else
    if a:flag > 0
      let content = zencoding#util#getContent(block)
      if content !~ '^{.*}$'
        let block = zencoding#util#searchRegion('{', '}')
        if zencoding#util#regionIsValid(block)
          call zencoding#util#selectRegion(block)
          return
        endif
      endif
    else
      let pos = searchpos('.*;', 'nW')
      if pos[0] != 0
        call setpos('.', [0, pos[0], pos[1], 0])
        let block = zencoding#util#searchRegion('^', ';')
        if zencoding#util#regionIsValid(block)
          call zencoding#util#selectRegion(block)
          return
        endif
      endif
    endif
  endif
  if a:flag == -2 || a:flag == 2
    silent! exe "normal! gv"
  else
    call setpos('.', curpos)
  endif
endfunction

function! zencoding#lang#css#moveNextPrev(flag)
  let pos = search('""\|()\|\(:\s*\zs$\)', a:flag ? 'Wbp' : 'Wp')
  if pos == 2
    startinsert!
  else
    silent! normal! l
    startinsert
  endif
endfunction

function! zencoding#lang#css#splitJoinTag()
  " nothing to do
endfunction

function! zencoding#lang#css#removeTag()
  " nothing to do
endfunction
