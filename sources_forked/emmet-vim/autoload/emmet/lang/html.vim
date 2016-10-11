let s:bx = '{\%("[^"]*"\|''[^'']*''\|\$#\|\${\w\+}\|\$\+\|{[^{]\+\|[^{}]\)\{-}}'
let s:mx = '\([+>]\|[<^]\+\)\{-}\s*'
\     .'\((*\)\{-}\s*'
\       .'\([@#.]\{-}[a-zA-Z_\!][a-zA-Z0-9:_\!\-$]*\|' . s:bx . '\|\[[^\]]\+\]\)'
\       .'\('
\         .'\%('
\           .'\%(#{[{}a-zA-Z0-9_\-\$]\+\|#[a-zA-Z0-9_\-\$]\+\)'
\           .'\|\%(\[\%("[^"]*"\|[^"\]]*\)\+\]\)'
\           .'\|\%(\.{[{}a-zA-Z0-9_\-\$]\+\|\.[a-zA-Z0-9_\-\$]\+\)'
\         .'\)*'
\       .'\)'
\       .'\%(\(' . s:bx . '\+\)\)\{0,1}'
\         .'\%(\(@-\{0,1}[0-9]*\)\{0,1}\*\([0-9]\+\)\)\{0,1}'
\     .'\(\%()\%(\(@-\{0,1}[0-9]*\)\{0,1}\*[0-9]\+\)\{0,1}\)*\)'

function! emmet#lang#html#findTokens(str) abort
  let str = a:str
  let [pos, last_pos] = [0, 0]
  while 1
    let tag = matchstr(str, '<[a-zA-Z].\{-}>', pos)
    if len(tag) == 0
      break
    endif
    let pos = stridx(str, tag, pos) + len(tag)
  endwhile
  while 1
    let tag = matchstr(str, '{%[^%]\{-}%}', pos)
    if len(tag) == 0
      break
    endif
    let pos = stridx(str, tag, pos) + len(tag)
  endwhile
  let last_pos = pos
  while len(str) > 0
    let token = matchstr(str, s:mx, pos)
    if token ==# ''
      break
    endif
    if token =~# '^\s'
      let token = matchstr(token, '^\s*\zs.*')
      let last_pos = stridx(str, token, pos)
    endif
    let pos = stridx(str, token, pos) + len(token)
  endwhile
  let str = a:str[last_pos :-1]
  if str =~# '^\w\+="[^"]*$'
    return ''
  endif
  return str
endfunction

function! emmet#lang#html#parseIntoTree(abbr, type) abort
  let abbr = a:abbr
  let type = a:type

  let settings = emmet#getSettings()
  if !has_key(settings, type)
    let type = 'html'
  endif
  if len(type) == 0 | let type = 'html' | endif

  let indent = emmet#getIndentation(type)
  let pmap = {
  \'p': 'span',
  \'ul': 'li',
  \'ol': 'li',
  \'table': 'tr',
  \'tr': 'td',
  \'tbody': 'tr',
  \'thead': 'tr',
  \'tfoot': 'tr',
  \'colgroup': 'col',
  \'select': 'option',
  \'optgroup': 'option',
  \'audio': 'source',
  \'video': 'source',
  \'object': 'param',
  \'map': 'area'
  \}

  let inlineLevel = split('a,abbr,acronym,applet,b,basefont,bdo,big,br,button,cite,code,del,dfn,em,font,i,iframe,img,input,ins,kbd,label,map,object,q,s,samp,select,small,span,strike,strong,sub,sup,textarea,tt,u,var',',')

  let custom_expands = emmet#getResource(type, 'custom_expands', {})
  if empty(custom_expands) && has_key(settings, 'custom_expands')
    let custom_expands = settings['custom_expands']
  endif

  " try 'foo' to (foo-x)
  let rabbr = emmet#getExpandos(type, abbr)
  if rabbr == abbr
    " try 'foo+(' to (foo-x)
    let rabbr = substitute(abbr, '\%(+\|^\)\([a-zA-Z][a-zA-Z0-9+]\+\)+\([(){}>]\|$\)', '\="(".emmet#getExpandos(type, submatch(1)).")".submatch(2)', 'i')
  endif
  let abbr = rabbr

  let root = emmet#newNode()
  let parent = root
  let last = root
  let pos = []
  while len(abbr)
    " parse line
    let match = matchstr(abbr, s:mx)
    let str = substitute(match, s:mx, '\0', 'ig')
    let operator = substitute(match, s:mx, '\1', 'ig')
    let block_start = substitute(match, s:mx, '\2', 'ig')
    let tag_name = substitute(match, s:mx, '\3', 'ig')
    let attributes = substitute(match, s:mx, '\4', 'ig')
    let value = substitute(match, s:mx, '\5', 'ig')
    let basevalue = substitute(match, s:mx, '\6', 'ig')
    let multiplier = 0 + substitute(match, s:mx, '\7', 'ig')
    let block_end = substitute(match, s:mx, '\8', 'ig')
    let custom = ''
    let important = 0
    if len(str) == 0
      break
    endif
    if tag_name =~# '^#'
      let attributes = tag_name . attributes
      let tag_name = ''
    endif
    if tag_name =~# '[^!]!$'
      let tag_name = tag_name[:-2]
      let important = 1
    endif
    if tag_name =~# '^\.'
      let attributes = tag_name . attributes
      let tag_name = ''
    endif
    if tag_name =~# '^\[.*\]$'
      let attributes = tag_name . attributes
      let tag_name = ''
    endif

    for k in keys(custom_expands)
      if tag_name =~ k
        let custom = tag_name
        let tag_name = ''
        break
      endif
    endfor

    if empty(tag_name)
      let pname = len(parent.child) > 0 ? parent.child[0].name : ''
      if !empty(pname) && has_key(pmap, pname)
        let tag_name = pmap[pname]
      elseif !empty(pname) && index(inlineLevel, pname) > -1
        let tag_name = 'span'
      elseif len(parent.child) == 0 || len(custom) == 0
        let tag_name = 'div'
      else
        let tag_name = custom
      endif
    endif

    let basedirect = basevalue[1] ==# '-' ? -1 : 1
    let basevalue = 0 + abs(basevalue[1:])
    if multiplier <= 0 | let multiplier = 1 | endif

    " make default node
    let current = emmet#newNode()

    let current.name = tag_name
    let current.important = important

    " aliases
    let aliases = emmet#getResource(type, 'aliases', {})
    if has_key(aliases, tag_name)
      let current.name = aliases[tag_name]
    endif

    let use_pipe_for_cursor = emmet#getResource(type, 'use_pipe_for_cursor', 1)

    " snippets
    let snippets = emmet#getResource(type, 'snippets', {})
    if !empty(snippets)
      let snippet_name = tag_name
      if has_key(snippets, snippet_name)
        let snippet = snippet_name
        while has_key(snippets, snippet)
          let snippet = snippets[snippet]
        endwhile
        if use_pipe_for_cursor
          let snippet = substitute(snippet, '|', '${cursor}', 'g')
        endif
        " just redirect to expanding
        if type == 'html' && snippet !~ '^\s*[{\[<]'
           return emmet#lang#html#parseIntoTree(snippet, a:type)
        endif
        let lines = split(snippet, "\n", 1)
        call map(lines, 'substitute(v:val, "\\(    \\|\\t\\)", escape(indent, "\\\\"), "g")')
        let current.snippet = join(lines, "\n")
        let current.name = ''
      endif
    endif

    for k in keys(custom_expands)
      if tag_name =~# k
        let current.snippet = '${' . (empty(custom) ? tag_name : custom) . '}'
        let current.name = ''
        break
      elseif custom =~# k
        let current.snippet = '${' . custom . '}'
        let current.name = ''
        break
      endif
    endfor

    " default_attributes
    let default_attributes = emmet#getResource(type, 'default_attributes', {})
    if !empty(default_attributes)
      for pat in [current.name, tag_name]
        if has_key(default_attributes, pat)
          if type(default_attributes[pat]) == 4
            let a = default_attributes[pat]
            let current.attrs_order += keys(a)
            if use_pipe_for_cursor
              for k in keys(a)
                let current.attr[k] = len(a[k]) ? substitute(a[k], '|', '${cursor}', 'g') : '${cursor}'
              endfor
            else
              for k in keys(a)
                let current.attr[k] = a[k]
              endfor
            endif
          else
            for a in default_attributes[pat]
              let current.attrs_order += keys(a)
              if use_pipe_for_cursor
                for k in keys(a)
                  let current.attr[k] = len(a[k]) ? substitute(a[k], '|', '${cursor}', 'g') : '${cursor}'
                endfor
              else
                for k in keys(a)
                  let current.attr[k] = a[k]
                endfor
              endif
            endfor
          endif
          if has_key(settings.html.default_attributes, current.name)
            let current.name = substitute(current.name, ':.*$', '', '')
          endif
          break
        endif
      endfor
    endif

    " parse attributes
    if len(attributes)
      let attr = attributes
      while len(attr)
        let item = matchstr(attr, '\(\%(\%(#[{}a-zA-Z0-9_\-\$]\+\)\|\%(\[\%("[^"]*"\|[^"\]]*\)\+\]\)\|\%(\.[{}a-zA-Z0-9_\-\$]\+\)*\)\)')
        if g:emmet_debug > 1
          echomsg 'attr=' . item
        endif
        if len(item) == 0
          break
        endif
        if item[0] ==# '#'
          let current.attr.id = item[1:]
        endif
        if item[0] ==# '.'
          let current.attr.class = substitute(item[1:], '\.', ' ', 'g')
        endif
        if item[0] ==# '['
          let atts = item[1:-2]
          if matchstr(atts, '^\s*\zs[0-9a-zA-Z_\-:]\+\(="[^"]*"\|=''[^'']*''\|=[^ ''"]\+\)') ==# ''
            let ks = []
			if has_key(default_attributes, current.name)
              let dfa = default_attributes[current.name]
              let ks = type(dfa) == 3 ? keys(dfa[0]) : keys(dfa)
            endif
            if len(ks) == 0 && has_key(default_attributes, current.name . ':src')
              let ks = keys(default_attributes[current.name . ':src'])
            endif
            if len(ks) > 0
              let current.attr[ks[0]] = atts
            else
              let current.attr[atts] = ''
            endif
          else
            while len(atts)
              let amat = matchstr(atts, '^\s*\zs\([0-9a-zA-Z-:]\+\%(="[^"]*"\|=''[^'']*''\|=[^ ''"]\+\|[^ ''"\]]*\)\{0,1}\)')
              if len(amat) == 0
                break
              endif
              let key = split(amat, '=')[0]
              let Val = amat[len(key)+1:]
              if key =~# '\.$' && Val ==# ''
                let key = key[:-2]
                unlet Val
                let Val = function('emmet#types#true')
              elseif Val =~# '^["'']'
                let Val = Val[1:-2]
              endif
              let current.attr[key] = Val
              if index(current.attrs_order, key) == -1
                let current.attrs_order += [key]
              endif
              let atts = atts[stridx(atts, amat) + len(amat):]
              unlet Val
            endwhile
          endif
        endif
        let attr = substitute(strpart(attr, len(item)), '^\s*', '', '')
      endwhile
    endif

    " parse text
    if tag_name =~# '^{.*}$'
      let current.name = ''
      let current.value = tag_name
    else
      let current.value = value
    endif
    let current.basedirect = basedirect
    let current.basevalue = basevalue
    let current.multiplier = multiplier

    " parse step inside/outside
    if !empty(last)
      if operator =~# '>'
        unlet! parent
        let parent = last
        let current.parent = last
        let current.pos = last.pos + 1
      else
        let current.parent = parent
        let current.pos = last.pos
      endif
    else
      let current.parent = parent
      let current.pos = 1
    endif
    if operator =~# '[<^]'
      for c in range(len(operator))
        let tmp = parent.parent
        if empty(tmp)
          break
        endif
        let parent = tmp
        let current.parent = tmp
      endfor
    endif

    call add(parent.child, current)
    let last = current

    " parse block
    if block_start =~# '('
      if operator =~# '>'
        let last.pos += 1
      endif
      let last.block = 1
      for n in range(len(block_start))
        let pos += [last.pos]
      endfor
    endif
    if block_end =~# ')'
      for n in split(substitute(substitute(block_end, ' ', '', 'g'), ')', ',),', 'g'), ',')
        if n ==# ')'
          if len(pos) > 0 && last.pos >= pos[-1]
            for c in range(last.pos - pos[-1])
              let tmp = parent.parent
              if !has_key(tmp, 'parent')
                break
              endif
              let parent = tmp
            endfor
            if len(pos) > 0
              call remove(pos, -1)
            endif
            let last = parent
            let last.pos += 1
          endif
        elseif len(n)
          let st = 0
          for nc in range(len(last.child))
            if last.child[nc].block
              let st = nc
              break
            endif
          endfor
          let cl = last.child[st :]
          let cls = []
          for c in range(n[1:])
            for cc in cl
              if cc.multiplier > 1
                let cc.basedirect = c + 1
              else
                let cc.basevalue = c + 1
              endif
            endfor
            let cls += deepcopy(cl)
          endfor
          if st > 0
            let last.child = last.child[:st-1] + cls
          else
            let last.child = cls
          endif
        endif
      endfor
    endif
    let abbr = abbr[stridx(abbr, match) + len(match):]

    if g:emmet_debug > 1
      echomsg 'str='.str
      echomsg 'block_start='.block_start
      echomsg 'tag_name='.tag_name
      echomsg 'operator='.operator
      echomsg 'attributes='.attributes
      echomsg 'value='.value
      echomsg 'basevalue='.basevalue
      echomsg 'multiplier='.multiplier
      echomsg 'block_end='.block_end
      echomsg 'abbr='.abbr
      echomsg 'pos='.string(pos)
      echomsg '---'
    endif
  endwhile
  return root
endfunction

function! s:dollar_add(base,no) abort
  if a:base > 0
    return a:base + a:no - 1
  elseif a:base < 0
    return a:base - a:no + 1
  else
    return a:no
  endif
endfunction

function! emmet#lang#html#toString(settings, current, type, inline, filters, itemno, indent) abort
  let settings = a:settings
  let current = a:current
  let type = a:type
  let inline = a:inline
  let filters = a:filters
  let itemno = a:itemno
  let indent = a:indent
  let dollar_expr = emmet#getResource(type, 'dollar_expr', 1)
  let q = emmet#getResource(type, 'quote_char', '"')
  let ct = emmet#getResource(type, 'comment_type', 'both')
  let an = emmet#getResource(type, 'attribute_name', {})

  if emmet#useFilter(filters, 'haml')
    return emmet#lang#haml#toString(settings, current, type, inline, filters, itemno, indent)
  endif
  if emmet#useFilter(filters, 'slim')
    return emmet#lang#slim#toString(settings, current, type, inline, filters, itemno, indent)
  endif

  let comment = ''
  let current_name = current.name
  if dollar_expr
    let current_name = substitute(current_name, '\$$', itemno+1, '')
  endif

  let str = ''
  if len(current_name) == 0
    let text = current.value[1:-2]
    if dollar_expr
      " TODO: regexp engine specified
      let nr = itemno + 1
      if exists('&regexpengine')
        let text = substitute(text, '\%#=1\%(\\\)\@\<!\(\$\+\)\(@-\?[0-9]\+\)\{0,1}\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d",s:dollar_add(submatch(2)[1:],nr)).submatch(3)', 'g')
      else
        let text = substitute(text, '\%(\\\)\@\<!\(\$\+\)\(@-\?[0-9]\+\)\{0,1}\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d",s:dollar_add(submatch(2)[1:],nr).submatch(3)', 'g')
      endif
      let text = substitute(text, '\${nr}', "\n", 'g')
      let text = substitute(text, '\\\$', '$', 'g')
    endif
    return text
  endif
  if len(current_name) > 0
    let str .= '<' . current_name
  endif
  for attr in emmet#util#unique(current.attrs_order + keys(current.attr))
    if !has_key(current.attr, attr)
      continue
    endif
    let Val = current.attr[attr]
    if type(Val) == 2 && Val == function('emmet#types#true')
      unlet Val
      let Val = 'true'
      if g:emmet_html5
        let str .= ' ' . attr
      else
        let str .= ' ' . attr . '=' . q . attr . q
      endif
      if emmet#useFilter(filters, 'c')
        if attr ==# 'id' | let comment .= '#' . Val | endif
        if attr ==# 'class' | let comment .= '.' . Val | endif
      endif
    else
      if dollar_expr
        while Val =~# '\$\([^#{]\|$\)'
          " TODO: regexp engine specified
          if exists('&regexpengine')
            let Val = substitute(Val, '\%#=1\(\$\+\)\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d", itemno+1).submatch(2)', 'g')
          else
            let Val = substitute(Val, '\(\$\+\)\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d", itemno+1).submatch(2)', 'g')
          endif
        endwhile
        let attr = substitute(attr, '\$$', itemno+1, '')
      endif
      if attr ==# 'class' && emmet#useFilter(filters, 'bem')
        let vals = split(Val, '\s\+')
        let Val = ''
        let lead = ''
        for _val in vals
          if len(Val) > 0
            let Val .= ' '
          endif
          if _val =~# '^_'
            if has_key(current.parent.attr, 'class')
              let lead = current.parent.attr["class"]
              if _val =~# '^__'
                let Val .= lead . _val
              else
                let Val .= lead . ' ' . lead . _val
              endif
            else
              let lead = split(vals[0], '_')[0]
              let Val .= lead . _val
            endif
          elseif _val =~# '^-'
            for l in split(_val, '_')
              if len(Val) > 0
                let Val .= ' '
              endif
              let l = substitute(l, '^-', '__', '')
              if len(lead) == 0
                let pattr = current.parent.attr
                if has_key(pattr, 'class')
                  let lead = split(pattr['class'], '\s\+')[0]
                endif
              endif
              let Val .= lead . l
              let lead .= l . '_'
            endfor
          else
            let Val .= _val
          endif
        endfor
      endif
      if has_key(an, attr)
        let attr = an[attr]
      endif
      if emmet#isExtends(type, 'jsx') && Val =~ '^{.*}$'
        let str .= ' ' . attr . '=' . Val
      else
        let str .= ' ' . attr . '=' . q . Val . q
      endif
      if emmet#useFilter(filters, 'c')
        if attr ==# 'id' | let comment .= '#' . Val | endif
        if attr ==# 'class' | let comment .= '.' . Val | endif
      endif
    endif
    unlet Val
  endfor
  if len(comment) > 0 && ct ==# 'both'
    let str = '<!-- ' . comment . " -->\n" . str
  endif
  if stridx(','.settings.html.empty_elements.',', ','.current_name.',') != -1
    let str .= settings.html.empty_element_suffix
  else
    let str .= '>'
    let text = current.value[1:-2]
    if dollar_expr
      " TODO: regexp engine specified
      let nr = itemno + 1
      if exists('&regexpengine')
        let text = substitute(text, '\%#=1\%(\\\)\@\<!\(\$\+\)\(@-\?[0-9]\+\)\{0,1}\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d",s:dollar_add(submatch(2)[1:],nr)).submatch(3)', 'g')
      else
        let text = substitute(text, '\%(\\\)\@\<!\(\$\+\)\(@-\?[0-9]\+\)\{0,1}\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d",s:dollar_add(submatch(2)[1:],nr)).submatch(3)', 'g')
      endif
      let text = substitute(text, '\${nr}', "\n", 'g')
      let text = substitute(text, '\\\$', '$', 'g')
      if text != ''
        let str = substitute(str, '\("\zs$#\ze"\|\s\zs\$#"\|"\$#\ze\s\)', text, 'g')
      endif
    endif
    let str .= text
    let nc = len(current.child)
    let dr = 0
    if nc > 0
      for n in range(nc)
        let child = current.child[n]
        if child.multiplier > 1
          let str .= "\n" . indent
          let dr = 1
        elseif len(current_name) > 0 && stridx(','.settings.html.inline_elements.',', ','.current_name.',') == -1
          if nc > 1 || (len(child.name) > 0 && stridx(','.settings.html.inline_elements.',', ','.child.name.',') == -1)
            let str .= "\n" . indent
            let dr = 1
          elseif current.multiplier == 1 && nc == 1 && len(child.name) == 0
            let str .= "\n" . indent
            let dr = 1
          endif
        endif
        let inner = emmet#toString(child, type, 0, filters, itemno, indent)
        let inner = substitute(inner, "^\n", '', 'g')
        let inner = substitute(inner, "\n", "\n" . escape(indent, '\'), 'g')
        let inner = substitute(inner, "\n" . escape(indent, '\') . '$', '', 'g')
        let str .= inner
      endfor
    else
      if settings.html.indent_blockelement && len(current_name) > 0 && stridx(','.settings.html.inline_elements.',', ','.current_name.',') == -1
        let str .= "\n" . indent . '${cursor}' . "\n"
      else
        let str .= '${cursor}'
      endif
    endif
    if dr
      let str .= "\n"
    endif
    let str .= '</' . current_name . '>'
  endif
  if len(comment) > 0
    if ct ==# 'lastonly'
      let str .= '<!-- ' . comment . ' -->'
    else
      let str .= "\n<!-- /" . comment . ' -->'
    endif
  endif
  if len(current_name) > 0 && current.multiplier > 0 || stridx(','.settings.html.block_elements.',', ','.current_name.',') != -1
    let str .= "\n"
  endif
  return str
endfunction

function! emmet#lang#html#imageSize() abort
  let img_region = emmet#util#searchRegion('<img\s', '>')
  if !emmet#util#regionIsValid(img_region) || !emmet#util#cursorInRegion(img_region)
    return
  endif
  let content = emmet#util#getContent(img_region)
  if content !~# '^<img[^><]\+>$'
    return
  endif
  let current = emmet#lang#html#parseTag(content)
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
  let html = substitute(emmet#toString(current, 'html', 1), '\n', '', '')
  let html = substitute(html, '\${cursor}', '', '')
  call emmet#util#setContent(img_region, html)
endfunction

function! emmet#lang#html#encodeImage() abort
  let img_region = emmet#util#searchRegion('<img\s', '>')
  if !emmet#util#regionIsValid(img_region) || !emmet#util#cursorInRegion(img_region)
    return
  endif
  let content = emmet#util#getContent(img_region)
  if content !~# '^<img[^><]\+>$'
    return
  endif
  let current = emmet#lang#html#parseTag(content)
  if empty(current) || !has_key(current.attr, 'src')
    return
  endif
  let fn = current.attr.src
  if fn !~# '^\(/\|http\)'
    let fn = simplify(expand('%:h') . '/' . fn)
  endif

  let [width, height] = emmet#util#getImageSize(fn)
  if width == -1 && height == -1
    return
  endif
  let current.attr.width = width
  let current.attr.height = height
  let html = emmet#toString(current, 'html', 1)
  call emmet#util#setContent(img_region, html)
endfunction

function! emmet#lang#html#parseTag(tag) abort
  let current = emmet#newNode()
  let mx = '<\([a-zA-Z][a-zA-Z0-9]*\)\(\%(\s[a-zA-Z][a-zA-Z0-9]\+=\%([^"'' \t]\+\|"[^"]\{-}"\|''[^'']\{-}''\)\s*\)*\)\(/\{0,1}\)>'
  let match = matchstr(a:tag, mx)
  let current.name = substitute(match, mx, '\1', 'i')
  let attrs = substitute(match, mx, '\2', 'i')
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

function! emmet#lang#html#toggleComment() abort
  let orgpos = getpos('.')
  let curpos = getpos('.')
  let mx = '<\%#[^>]*>'
  while 1
    let block = emmet#util#searchRegion('<!--', '-->')
    if emmet#util#regionIsValid(block)
      let block[1][1] += 2
      let content = emmet#util#getContent(block)
      let content = substitute(content, '^<!--\s\(.*\)\s-->$', '\1', '')
      call emmet#util#setContent(block, content)
      silent! call setpos('.', orgpos)
      return
    endif
    let block = emmet#util#searchRegion('<[^>]', '>')
    if !emmet#util#regionIsValid(block)
      let pos1 = searchpos('<', 'bcW')
      if pos1[0] == 0 && pos1[1] == 0
        return
      endif
      let curpos = getpos('.')
      continue
    endif
    let pos1 = block[0]
    let pos2 = block[1]
    let content = emmet#util#getContent(block)
    let tag_name = matchstr(content, '^<\zs/\{0,1}[^ \r\n>]\+')
    if tag_name[0] ==# '/'
      call setpos('.', [0, pos1[0], pos1[1], 0])
      let pos2 = searchpairpos('<'. tag_name[1:] . '\>[^>]*>', '', '</' . tag_name[1:] . '>', 'bnW')
      let pos1 = searchpos('>', 'cneW')
      let block = [pos2, pos1]
    elseif tag_name =~# '/$'
      if !emmet#util#pointInRegion(orgpos[1:2], block)
        " it's broken tree
        call setpos('.', orgpos)
        let block = emmet#util#searchRegion('>', '<')
        let content = '><!-- ' . emmet#util#getContent(block)[1:-2] . ' --><'
        call emmet#util#setContent(block, content)
        silent! call setpos('.', orgpos)
        return
      endif
    else
      call setpos('.', [0, pos2[0], pos2[1], 0])
      let pos3 = searchpairpos('<'. tag_name . '\>[^>]*>', '', '</' . tag_name . '>', 'nW')
      if pos3 == [0, 0]
        let block = [pos1, pos2]
      else
        call setpos('.', [0, pos3[0], pos3[1], 0])
        let pos2 = searchpos('>', 'neW')
        let block = [pos1, pos2]
      endif
    endif
    if !emmet#util#regionIsValid(block)
      silent! call setpos('.', orgpos)
      return
    endif
    if emmet#util#pointInRegion(curpos[1:2], block)
      let content = '<!-- ' . emmet#util#getContent(block) . ' -->'
      call emmet#util#setContent(block, content)
      silent! call setpos('.', orgpos)
      return
    endif
  endwhile
endfunction

function! emmet#lang#html#balanceTag(flag) range abort
  let vblock = emmet#util#getVisualBlock()
  if a:flag == -2 || a:flag == 2
    let curpos = [0, line("'<"), col("'<"), 0]
  else
    let curpos = emmet#util#getcurpos()
  endif
  let settings = emmet#getSettings()

  if a:flag > 0
    let mx = '<\([a-zA-Z][a-zA-Z0-9:_\-]*\)[^>]*'
    let last = curpos[1:2]
    while 1
      let pos1 = searchpos(mx, 'bW')
      let content = matchstr(getline(pos1[0])[pos1[1]-1:], mx)
      let tag_name = matchstr(content, '^<\zs[a-zA-Z0-9:_\-]*\ze')
      if stridx(','.settings.html.empty_elements.',', ','.tag_name.',') != -1
        let pos2 = searchpos('>', 'nW')
      else
        let pos2 = searchpairpos('<' . tag_name . '[^>]*>', '', '</'. tag_name . '\zs>', 'nW')
      endif
      let block = [pos1, pos2]
      if pos1[0] == 0 && pos1[1] == 0
        break
      endif
      if emmet#util#pointInRegion(last, block) && emmet#util#regionIsValid(block)
        call emmet#util#selectRegion(block)
        return
      endif
      if pos1 == last
        break
      endif
      let last = pos1
    endwhile
  else
    let mx = '<\([a-zA-Z][a-zA-Z0-9:_\-]*\)[^>]*>'
    while 1
      let pos1 = searchpos(mx, 'W')
      if pos1 == curpos[1:2]
        let pos1 = searchpos(mx . '\zs', 'W')
        let pos2 = searchpos('.\ze<', 'W')
        let block = [pos1, pos2]
        if emmet#util#regionIsValid(block)
          call emmet#util#selectRegion(block)
          return
        endif
      endif
      let content = matchstr(getline(pos1[0])[pos1[1]-1:], mx)
      let tag_name = matchstr(content, '^<\zs[a-zA-Z0-9:_\-]*\ze')
      if stridx(','.settings.html.empty_elements.',', ','.tag_name.',') != -1
        let pos2 = searchpos('>', 'nW')
      else
        let pos2 = searchpairpos('<' . tag_name . '[^>]*>', '', '</'. tag_name . '\zs>', 'nW')
      endif
      let block = [pos1, pos2]
      if pos1[0] == 0 && pos1[1] == 0
        break
      endif
      if emmet#util#regionIsValid(block)
        call emmet#util#selectRegion(block)
        return
      endif
    endwhile
  endif
  if a:flag == -2 || a:flag == 2
    silent! exe 'normal! gv'
  else
    call setpos('.', curpos)
  endif
endfunction

function! emmet#lang#html#moveNextPrevItem(flag) abort
  silent! exe "normal \<esc>"
  let mx = '\%([0-9a-zA-Z-:]\+\%(="[^"]*"\|=''[^'']*''\|[^ ''">\]]*\)\{0,1}\)'
  let pos = searchpos('\s'.mx.'\zs', '')
  if pos != [0,0]
    call feedkeys('v?\s\zs'.mx."\<cr>", '')
  endif
endfunction

function! emmet#lang#html#moveNextPrev(flag) abort
  let pos = search('\%(</\w\+\)\@<!\zs><\/\|\(""\)\|^\(\s*\)$', a:flag ? 'Wpb' : 'Wp')
  if pos == 3
    startinsert!
  elseif pos != 0
    silent! normal! l
    startinsert
  endif
endfunction

function! emmet#lang#html#splitJoinTag() abort
  let curpos = emmet#util#getcurpos()
  while 1
    let mx = '<\(/\{0,1}[a-zA-Z][a-zA-Z0-9:_\-]*\)[^>]*>'
    let pos1 = searchpos(mx, 'bcnW')
    let content = matchstr(getline(pos1[0])[pos1[1]-1:], mx)
    let tag_name = substitute(content, '^<\(/\{0,1}[a-zA-Z][a-zA-Z0-9:_\-]*\).*$', '\1', '')
    let block = [pos1, [pos1[0], pos1[1] + len(content) - 1]]
    if content[-2:] ==# '/>' && emmet#util#cursorInRegion(block)
      let content = substitute(content[:-3], '\s*$', '', '')  . '></' . tag_name . '>'
      call emmet#util#setContent(block, content)
      call setpos('.', [0, block[0][0], block[0][1], 0])
      return
    else
      if tag_name[0] ==# '/'
        let pos1 = searchpos('<' . tag_name[1:] . '[^a-zA-Z0-9]', 'bcnW')
        call setpos('.', [0, pos1[0], pos1[1], 0])
        let pos2 = searchpos('</' . tag_name[1:] . '>', 'cneW')
      else
        let pos2 = searchpos('</' . tag_name . '>', 'cneW')
      endif
      let block = [pos1, pos2]
      let content = emmet#util#getContent(block)
      if emmet#util#pointInRegion(curpos[1:2], block) && content[1:] !~# '<' . tag_name . '[^a-zA-Z0-9]*[^>]*>'
        let content = matchstr(content, mx)[:-2] . ' />'
        call emmet#util#setContent(block, content)
        call setpos('.', [0, block[0][0], block[0][1], 0])
        return
      else
        if block[0][0] > 0
          call setpos('.', [0, block[0][0]-1, block[0][1], 0])
        else
          call setpos('.', curpos)
          return
        endif
      endif
    endif
  endwhile
endfunction

function! emmet#lang#html#removeTag() abort
  let curpos = emmet#util#getcurpos()
  while 1
    let mx = '<\(/\{0,1}[a-zA-Z][a-zA-Z0-9:_\-]*\)[^>]*'
    let pos1 = searchpos(mx, 'bcnW')
    let content = matchstr(getline(pos1[0])[pos1[1]-1:], mx)
    let tag_name = matchstr(content, '^<\zs/\{0,1}[a-zA-Z0-9:_\-]*')
    let block = [pos1, [pos1[0], pos1[1] + len(content) - 1]]
    if content[-2:] ==# '/>' && emmet#util#cursorInRegion(block)
      call emmet#util#setContent(block, '')
      call setpos('.', [0, block[0][0], block[0][1], 0])
      return
    else
      if tag_name[0] ==# '/'
        let pos1 = searchpos('<' . tag_name[1:] . '[^a-zA-Z0-9]', 'bcnW')
        call setpos('.', [0, pos1[0], pos1[1], 0])
        let pos2 = searchpos('</' . tag_name[1:] . '>', 'cneW')
      else
        let pos2 = searchpos('</' . tag_name . '>', 'cneW')
      endif
      let block = [pos1, pos2]
      let content = emmet#util#getContent(block)
      if emmet#util#pointInRegion(curpos[1:2], block) && content[1:] !~# '^<' . tag_name . '[^a-zA-Z0-9]'
        call emmet#util#setContent(block, '')
        call setpos('.', [0, block[0][0], block[0][1], 0])
        return
      else
        if block[0][0] > 0
          call setpos('.', [0, block[0][0]-1, block[0][1], 0])
        else
          call setpos('.', curpos)
          return
        endif
      endif
    endif
  endwhile
endfunction
