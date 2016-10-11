function! emmet#lang#css#findTokens(str) abort
  let tmp = substitute(substitute(a:str, '^.*[;{]\s*', '', ''), '}\s*$', '', '')
  if tmp =~ '/' && tmp =~ '^[a-zA-Z0-9/_.]\+$'
    " maybe path or something
    return ''
  endif
  return substitute(substitute(a:str, '^.*[;{]\s*', '', ''), '}\s*$', '', '')
endfunction

function! emmet#lang#css#parseIntoTree(abbr, type) abort
  let abbr = a:abbr
  let type = a:type
  let prefix = 0
  let value = ''

  let indent = emmet#getIndentation(type)
  let aliases = emmet#getResource(type, 'aliases', {})
  let snippets = emmet#getResource(type, 'snippets', {})
  let use_pipe_for_cursor = emmet#getResource(type, 'use_pipe_for_cursor', 1)

  let root = emmet#newNode()

  " emmet
  let tokens = split(abbr, '+\ze[^+)!]')
  let block = emmet#util#searchRegion('{', '}')
  if abbr !~# '^@' && emmet#getBaseType(type) ==# 'css' && type !=# 'sass' && block[0] ==# [0,0] && block[1] ==# [0,0]
    let current = emmet#newNode()
    let current.snippet = substitute(abbr, '\s\+$', '', '') . " {\n" . indent . "${cursor}\n}"
    let current.name = ''
    call add(root.child, deepcopy(current))
  else
    for n in range(len(tokens))
      let token = tokens[n]
      let prop = matchlist(token, '^\(-\{0,1}[a-zA-Z]\+\|[a-zA-Z0-9]\++\{0,1}\|([a-zA-Z0-9]\++\{0,1})\)\(\%([0-9.-]\+\%(p\|e\|em\|re\|rem\|%\)\{0,1}-\{0,1}\|-auto\)*\)$')
      if len(prop)
        let token = substitute(prop[1], '^(\(.*\))', '\1', '')
        if token =~# '^-'
          let prefix = 1
          let token = token[1:]
        endif
        let value = ''
        for v in split(prop[2], '\d\zs-')
          if len(value) > 0
            let value .= ' '
          endif
          if token =~# '^[z]'
            " TODO
            let value .= substitute(v, '[^0-9.]*$', '', '')
          elseif v =~# 'p$'
            let value .= substitute(v, 'p$', '%', '')
          elseif v =~# '%$'
            let value .= v
          elseif v =~# 'e$'
            let value .= substitute(v, 'e$', 'em', '')
          elseif v =~# 'em$'
            let value .= v
          elseif v =~# 're$'
            let value .= substitute(v, 're$', 'rem', '')
          elseif v =~# 'rem$'
            let value .= v
          elseif v =~# '\.'
            let value .= v . 'em'
          elseif v ==# 'auto'
            let value .= v
          elseif v ==# '0'
            let value .= '0'
          else
            let value .= v . 'px'
          endif
        endfor
      endif

      let tag_name = token
      if tag_name =~# '.!$'
        let tag_name = tag_name[:-2]
        let important = 1
      else
        let important = 0
      endif
      " make default node
      let current = emmet#newNode()
      let current.important = important
      let current.name = tag_name

      " aliases
      if has_key(aliases, tag_name)
        let current.name = aliases[tag_name]
      endif

      " snippets
      if !empty(snippets)
        let snippet_name = tag_name
        if !has_key(snippets, snippet_name)
          let pat = '^' . join(split(tag_name, '\zs'), '\%(\|[^:-]\+-\)')
          let vv = filter(sort(keys(snippets)), 'snippets[v:val] =~ pat')
          if len(vv) > 0
            let snippet_name = vv[0]
          else
            let pat = '^' . join(split(tag_name, '\zs'), '\%(\|[^:-]\+-*\)')
            let vv = filter(sort(keys(snippets)), 'snippets[v:val] =~ pat')
            if len(vv) == 0
              let pat = '^' . join(split(tag_name, '\zs'), '[^:]\{-}')
              let vv = filter(sort(keys(snippets)), 'snippets[v:val] =~ pat')
              if len(vv) == 0
                let pat = '^' . join(split(tag_name, '\zs'), '.\{-}')
                let vv = filter(sort(keys(snippets)), 'snippets[v:val] =~ pat')
              endif
            endif
            let minl = -1
            for vk in vv
              let vvs = snippets[vk]
              if minl == -1 || len(vvs) < minl
                let snippet_name = vk
                let minl = len(vvs)
              endif
            endfor
          endif
        endif
        if has_key(snippets, snippet_name)
          let snippet = snippets[snippet_name]
          if use_pipe_for_cursor
            let snippet = substitute(snippet, '|', '${cursor}', 'g')
          endif
          let lines = split(snippet, "\n")
          call map(lines, 'substitute(v:val, "\\(    \\|\\t\\)", escape(indent, "\\\\"), "g")')
          let current.snippet = join(lines, "\n")
          let current.name = ''
          let current.snippet = substitute(current.snippet, ';', value . ';', '')
          if use_pipe_for_cursor && len(value) > 0
            let current.snippet = substitute(current.snippet, '\${cursor}', '', 'g')
          endif
          if n < len(tokens) - 1
            let current.snippet .= "\n"
          endif
        endif
      endif

      let current.pos = 0
      let lg = matchlist(token, '^\%(linear-gradient\|lg\)(\s*\(\S\+\)\s*,\s*\([^,]\+\)\s*,\s*\([^)]\+\)\s*)$')
      if len(lg) == 0
        let lg = matchlist(token, '^\%(linear-gradient\|lg\)(\s*\(\S\+\)\s*,\s*\([^,]\+\)\s*)$')
        if len(lg)
          let [lg[1], lg[2], lg[3]] = ['linear', lg[1], lg[2]]
        endif
      endif
      if len(lg)
        let current.name = ''
        let current.snippet = printf("background-image:-webkit-gradient(%s, 0 0, 0 100%, from(%s), to(%s));\n", lg[1], lg[2], lg[3])
        call add(root.child, deepcopy(current))
        let current.snippet = printf("background-image:-webkit-linear-gradient(%s, %s);\n", lg[2], lg[3])
        call add(root.child, deepcopy(current))
        let current.snippet = printf("background-image:-moz-linear-gradient(%s, %s);\n", lg[2], lg[3])
        call add(root.child, deepcopy(current))
        let current.snippet = printf("background-image:-o-linear-gradient(%s, %s);\n", lg[2], lg[3])
        call add(root.child, deepcopy(current))
        let current.snippet = printf("background-image:linear-gradient(%s, %s);\n", lg[2], lg[3])
        call add(root.child, deepcopy(current))
      elseif prefix
        let snippet = current.snippet
        let current.snippet = '-webkit-' . snippet . "\n"
        call add(root.child, deepcopy(current))
        let current.snippet = '-moz-' . snippet . "\n"
        call add(root.child, deepcopy(current))
        let current.snippet = '-o-' . snippet . "\n"
        call add(root.child, deepcopy(current))
        let current.snippet = '-ms-' . snippet . "\n"
        call add(root.child, deepcopy(current))
        let current.snippet = snippet
        call add(root.child, current)
      elseif token =~# '^c#\([0-9a-fA-F]\{3}\|[0-9a-fA-F]\{6}\)\(\.[0-9]\+\)\?'
        let cs = split(token, '\.')
        let current.name = ''
        let [r,g,b] = [0,0,0]
        if len(cs[0]) == 5
          let rgb = matchlist(cs[0], 'c#\(.\)\(.\)\(.\)')
          let r = eval('0x'.rgb[1].rgb[1])
          let g = eval('0x'.rgb[2].rgb[2])
          let b = eval('0x'.rgb[3].rgb[3])
        elseif len(cs[0]) == 8
          let rgb = matchlist(cs[0], 'c#\(..\)\(..\)\(..\)')
          let r = eval('0x'.rgb[1])
          let g = eval('0x'.rgb[2])
          let b = eval('0x'.rgb[3])
        endif
        if len(cs) == 1
          let current.snippet = printf('color:rgb(%d, %d, %d);', r, g, b)
        else
          let current.snippet = printf('color:rgb(%d, %d, %d, %s);', r, g, b, string(str2float('0.'.cs[1])))
        endif
        call add(root.child, current)
      elseif token =~# '^c#'
        let current.name = ''
        let current.snippet = 'color:\${cursor};'
        call add(root.child, current)
      else
        call add(root.child, current)
      endif
    endfor
  endif
  return root
endfunction

function! emmet#lang#css#toString(settings, current, type, inline, filters, itemno, indent) abort
  let current = a:current
  let value = current.value[1:-2]
  let tmp = substitute(value, '\${cursor}', '', 'g')
  if tmp !~ '.*{[ \t\r\n]*}$'
    if emmet#useFilter(a:filters, 'fc')
      let value = substitute(value, '\([^:]\+\):\([^;]*\)', '\1: \2', 'g')
    else
      let value = substitute(value, '\([^:]\+\):\([^;]*\)', '\1:\2', 'g')
    endif
    if current.important
      let value = substitute(value, ';', ' !important;', '')
    endif
  endif
  return value
endfunction

function! emmet#lang#css#imageSize() abort
  let img_region = emmet#util#searchRegion('{', '}')
  if !emmet#util#regionIsValid(img_region) || !emmet#util#cursorInRegion(img_region)
    return
  endif
  let content = emmet#util#getContent(img_region)
  let fn = matchstr(content, '\<url(\zs[^)]\+\ze)')
  let fn = substitute(fn, '[''" \t]', '', 'g')
  if fn =~# '^\s*$'
    return
  elseif fn !~# '^\(/\|http\)'
    let fn = simplify(expand('%:h') . '/' . fn)
  endif
  let [width, height] = emmet#util#getImageSize(fn)
  if width == -1 && height == -1
    return
  endif
  let indent = emmet#getIndentation('css')
  if content =~# '.*\<width\s*:[^;]*;.*'
    let content = substitute(content, '\<width\s*:[^;]*;', 'width: ' . width . 'px;', '')
  else
    let content = substitute(content, '}', indent . 'width: ' . width . "px;\n}", '')
  endif
  if content =~# '.*\<height\s*:[^;]*;.*'
    let content = substitute(content, '\<height\s*:[^;]*;', 'height: ' . height . 'px;', '')
  else
    let content = substitute(content, '}', indent . 'height: ' . height . "px;\n}", '')
  endif
  call emmet#util#setContent(img_region, content)
endfunction

function! emmet#lang#css#encodeImage() abort
endfunction

function! emmet#lang#css#parseTag(tag) abort
  return {}
endfunction

function! emmet#lang#css#toggleComment() abort
  let line = getline('.')
  let mx = '^\(\s*\)/\*\s*\(.*\)\s*\*/\s*$'
  if line =~# '{\s*$'
    let block = emmet#util#searchRegion('/\*', '\*/\zs')
    if emmet#util#regionIsValid(block)
      let content = emmet#util#getContent(block)
      let content = substitute(content, '/\*\s\(.*\)\s\*/', '\1', '')
      call emmet#util#setContent(block, content)
    else
      let node = expand('<cword>')
      if len(node)
        exe "normal ciw\<c-r>='/* '.node.' */'\<cr>"
      endif
    endif
  else
    if line =~# mx
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

function! emmet#lang#css#balanceTag(flag) range abort
  if a:flag == -2 || a:flag == 2
    let curpos = [0, line("'<"), col("'<"), 0]
  else
    let curpos = emmet#util#getcurpos()
  endif
  let block = emmet#util#getVisualBlock()
  if !emmet#util#regionIsValid(block)
    if a:flag > 0
      let block = emmet#util#searchRegion('^', ';')
      if emmet#util#regionIsValid(block)
        call emmet#util#selectRegion(block)
        return
      endif
    endif
  else
    if a:flag > 0
      let content = emmet#util#getContent(block)
      if content !~# '^{.*}$'
        let block = emmet#util#searchRegion('{', '}')
        if emmet#util#regionIsValid(block)
          call emmet#util#selectRegion(block)
          return
        endif
      endif
    else
      let pos = searchpos('.*;', 'nW')
      if pos[0] != 0
        call setpos('.', [0, pos[0], pos[1], 0])
        let block = emmet#util#searchRegion('^', ';')
        if emmet#util#regionIsValid(block)
          call emmet#util#selectRegion(block)
          return
        endif
      endif
    endif
  endif
  if a:flag == -2 || a:flag == 2
    silent! exe 'normal! gv'
  else
    call setpos('.', curpos)
  endif
endfunction

function! emmet#lang#css#moveNextPrevItem(flag) abort
  return emmet#lang#css#moveNextPrev(a:flag)
endfunction

function! emmet#lang#css#moveNextPrev(flag) abort
  let pos = search('""\|()\|\(:\s*\zs$\)', a:flag ? 'Wbp' : 'Wp')
  if pos == 2
    startinsert!
  else
    silent! normal! l
    startinsert
  endif
endfunction

function! emmet#lang#css#splitJoinTag() abort
  " nothing to do
endfunction

function! emmet#lang#css#removeTag() abort
  " nothing to do
endfunction
