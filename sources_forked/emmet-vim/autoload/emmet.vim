"=============================================================================
" emmet.vim
" Author: Yasuhiro Matsumoto <mattn.jp@gmail.com>
" Last Change: 26-Jul-2015.

let s:save_cpo = &cpoptions
set cpoptions&vim

let s:filtermx = '|\(\%(bem\|html\|haml\|slim\|e\|c\|s\|fc\|xsl\|t\|\/[^ ]\+\)\s*,\{0,1}\s*\)*$'

function! emmet#getExpandos(type, key) abort
  let expandos = emmet#getResource(a:type, 'expandos', {})
  if has_key(expandos, a:key)
    return expandos[a:key]
  endif
  return a:key
endfunction

function! emmet#splitFilterArg(filters) abort
  for f in a:filters
    if f =~# '^/'
      return f[1:]
    endif
  endfor
  return ''
endfunction

function! emmet#useFilter(filters, filter) abort
  for f in a:filters
    if a:filter ==# '/' && f =~# '^/'
      return 1
    elseif f ==# a:filter
      return 1
    endif
  endfor
  return 0
endfunction

function! emmet#getIndentation(...) abort
  if a:0 > 0
    let type = a:1
  else
    let type = emmet#getFileType()
  endif
  if has_key(s:emmet_settings, type) && has_key(s:emmet_settings[type], 'indentation')
    let indent = s:emmet_settings[type].indentation
  elseif has_key(s:emmet_settings, 'indentation')
    let indent = s:emmet_settings.indentation
  elseif has_key(s:emmet_settings.variables, 'indentation')
    let indent = s:emmet_settings.variables.indentation
  else
    let sw = exists('*shiftwidth') ? shiftwidth() : &l:shiftwidth
    let indent = (&l:expandtab || &l:tabstop !=# sw) ? repeat(' ', sw) : "\t"
  endif
  return indent
endfunction

function! emmet#getBaseType(type) abort
  if !has_key(s:emmet_settings, a:type)
    return ''
  endif
  if !has_key(s:emmet_settings[a:type], 'extends')
    return a:type
  endif
  let extends = s:emmet_settings[a:type].extends
  if type(extends) ==# 1
    let tmp = split(extends, '\s*,\s*')
    let ext = tmp[0]
  else
    let ext = extends[0]
  endif
  if a:type !=# ext
    return emmet#getBaseType(ext)
  endif
  return ''
endfunction

function! emmet#isExtends(type, extend) abort
  if a:type ==# a:extend
    return 1
  endif
  if !has_key(s:emmet_settings, a:type)
    return 0
  endif
  if !has_key(s:emmet_settings[a:type], 'extends')
    return 0
  endif
  let extends = s:emmet_settings[a:type].extends
  if type(extends) ==# 1
    let tmp = split(extends, '\s*,\s*')
    unlet! extends
    let extends = tmp
  endif
  for ext in extends
    if a:extend ==# ext
      return 1
    endif
  endfor
  return 0
endfunction

function! emmet#parseIntoTree(abbr, type) abort
  let abbr = a:abbr
  let type = a:type
  let rtype = emmet#lang#exists(type) ? type : 'html'
  return emmet#lang#{rtype}#parseIntoTree(abbr, type)
endfunction

function! emmet#expandAbbrIntelligent(feedkey) abort
  if !emmet#isExpandable()
    return a:feedkey
  endif
  return "\<plug>(emmet-expand-abbr)"
endfunction

function! emmet#isExpandable() abort
  let line = getline('.')
  if col('.') < len(line)
    let line = matchstr(line, '^\(.*\%'.col('.').'c\)')
  endif
  let part = matchstr(line, '\(\S.*\)$')
  let type = emmet#getFileType()
  let ftype = emmet#lang#exists(type) ? type : 'html'
  let part = emmet#lang#{ftype}#findTokens(part)
  return len(part) > 0
endfunction

function! emmet#mergeConfig(lhs, rhs) abort
  let [lhs, rhs] = [a:lhs, a:rhs]
  if type(lhs) ==# 3
    if type(rhs) ==# 3
      let lhs += rhs
      if len(lhs)
        call remove(lhs, 0, len(lhs)-1)
      endif
      for rhi in rhs
        call add(lhs, rhs[rhi])
      endfor
    elseif type(rhs) ==# 4
      let lhs += map(keys(rhs), '{v:val : rhs[v:val]}')
    endif
  elseif type(lhs) ==# 4
    if type(rhs) ==# 3
      for V in rhs
        if type(V) != 4
          continue
        endif
        for k in keys(V)
          let lhs[k] = V[k]
        endfor
      endfor
    elseif type(rhs) ==# 4
      for key in keys(rhs)
        if type(rhs[key]) ==# 3
          if !has_key(lhs, key)
            let lhs[key] = []
          endif
          if type(lhs[key]) == 3
            let lhs[key] += rhs[key]
          elseif type(lhs[key]) == 4
            for k in keys(rhs[key])
              let lhs[key][k] = rhs[key][k]
            endfor
          endif
        elseif type(rhs[key]) ==# 4
          if has_key(lhs, key)
            call emmet#mergeConfig(lhs[key], rhs[key])
          else
            let lhs[key] = rhs[key]
          endif
        else
          let lhs[key] = rhs[key]
        endif
      endfor
    endif
  endif
endfunction

function! emmet#newNode() abort
  return { 'name': '', 'attr': {}, 'child': [], 'snippet': '', 'basevalue': 0, 'basedirect': 1, 'multiplier': 1, 'parent': {}, 'value': '', 'pos': 0, 'important': 0, 'attrs_order': ['id', 'class'], 'block': 0 }
endfunction

function! s:itemno(itemno, current) abort
  let current = a:current
  if current.basedirect > 0
    if current.basevalue ==# 0
      return a:itemno
    else
      return current.basevalue - 1 + a:itemno
    endif
  else
    if current.basevalue ==# 0
      return current.multiplier - 1 - a:itemno
    else
      return current.multiplier + current.basevalue - 2 - a:itemno
    endif
  endif
endfunction

function! emmet#toString(...) abort
  let current = a:1
  if a:0 > 1
    let type = a:2
  else
    let type = &filetype
  endif
  if len(type) ==# 0 | let type = 'html' | endif
  if a:0 > 2
    let inline = a:3
  else
    let inline = 0
  endif
  if a:0 > 3
    if type(a:4) ==# 1
      let filters = split(a:4, '\s*,\s*')
    else
      let filters = a:4
    endif
  else
    let filters = ['html']
  endif
  if a:0 > 4
    let group_itemno = a:5
  else
    let group_itemno = 0
  endif
  if a:0 > 5
    let indent = a:6
  else
    let indent = ''
  endif

  let dollar_expr = emmet#getResource(type, 'dollar_expr', 1)
  let itemno = 0
  let str = ''
  let rtype = emmet#lang#exists(type) ? type : 'html'
  while itemno < current.multiplier
    if len(current.name)
      if current.multiplier ==# 1
        let inner = emmet#lang#{rtype}#toString(s:emmet_settings, current, type, inline, filters, s:itemno(group_itemno, current), indent)
      else
        let inner = emmet#lang#{rtype}#toString(s:emmet_settings, current, type, inline, filters, s:itemno(itemno, current), indent)
      endif
      if current.multiplier > 1
        let inner = substitute(inner, '\$#', '$line'.(itemno+1).'$', 'g')
      endif
      let str .= inner
    else
      let snippet = current.snippet
      if len(snippet) ==# 0
        let snippets = emmet#getResource(type, 'snippets', {})
        if !empty(snippets) && has_key(snippets, 'emmet_snippet')
          let snippet = snippets['emmet_snippet']
        endif
      endif
      if len(snippet) > 0
        let tmp = snippet
        let tmp = substitute(tmp, '\${emmet_name}', current.name, 'g')
        let snippet_node = emmet#newNode()
        let snippet_node.value = '{'.tmp.'}'
        let snippet_node.important = current.important
        let snippet_node.multiplier = current.multiplier
        let str .= emmet#lang#{rtype}#toString(s:emmet_settings, snippet_node, type, inline, filters, s:itemno(group_itemno, current), indent)
        if current.multiplier > 1
          let str .= "\n"
        endif
      else
        if len(current.name)
          let str .= current.name
        endif
        if len(current.value)
          let text = current.value[1:-2]
          if dollar_expr
            " TODO: regexp engine specified
            if exists('&regexpengine')
              let text = substitute(text, '\%#=1\%(\\\)\@\<!\(\$\+\)\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d", max([itemno, group_itemno])+1).submatch(2)', 'g')
            else
              let text = substitute(text, '\%(\\\)\@\<!\(\$\+\)\([^{#]\|$\)', '\=printf("%0".len(submatch(1))."d", max([itemno, group_itemno])+1).submatch(2)', 'g')
            endif
            let text = substitute(text, '\${nr}', "\n", 'g')
            let text = substitute(text, '\\\$', '$', 'g')
          endif
          let str .= text
        endif
      endif
      let inner = ''
      if len(current.child)
        for n in current.child
          let inner .= emmet#toString(n, type, inline, filters, s:itemno(group_itemno, n), indent)
        endfor
      else
        let inner = current.value[1:-2]
      endif
      let inner = substitute(inner, "\n", "\n" . indent, 'g')
      let str = substitute(str, '\${child}', inner, '')
    endif
    let itemno = itemno + 1
  endwhile
  return str
endfunction

function! emmet#getSettings() abort
  return s:emmet_settings
endfunction

function! emmet#getFilters(type) abort
  let filterstr = emmet#getResource(a:type, 'filters', '')
  return split(filterstr, '\s*,\s*')
endfunction

function! emmet#getResource(type, name, default) abort
  if exists('b:emmet_' . a:name)
    return get(b:, 'emmet_' . a:name)
  endif
  let global = {}
  if has_key(s:emmet_settings, '*') && has_key(s:emmet_settings['*'], a:name)
    let global = extend(global, s:emmet_settings['*'][a:name])
  endif

  if has_key(s:emmet_settings, a:type)
    let types = [a:type]
  else
    let types = split(a:type, '\.')
  endif

  for type in types
    if !has_key(s:emmet_settings, type)
      continue
    endif
    let ret = a:default

    if has_key(s:emmet_settings[type], 'extends')
      let extends = s:emmet_settings[type].extends
      if type(extends) ==# 1
        let tmp = split(extends, '\s*,\s*')
        unlet! extends
        let extends = tmp
      endif
      for ext in extends
        if has_key(s:emmet_settings, ext) && has_key(s:emmet_settings[ext], a:name)
          if type(ret) ==# 3 || type(ret) ==# 4
            call emmet#mergeConfig(ret, s:emmet_settings[ext][a:name])
          else
            let ret = s:emmet_settings[ext][a:name]
          endif
        endif
      endfor
    endif

    if has_key(s:emmet_settings[type], a:name)
      if type(ret) ==# 3 || type(ret) ==# 4
        call emmet#mergeConfig(ret, s:emmet_settings[type][a:name])
        return extend(global, ret)
      else
        return s:emmet_settings[type][a:name]
      endif
    endif
    if !empty(ret)
      if type(ret) ==# 3 || type(ret) ==# 4
        let ret = extend(global, ret)
      endif
      return ret
    endif
  endfor

  let ret = a:default
  if type(ret) ==# 3 || type(ret) ==# 4
    let ret = extend(global, ret)
  endif
  return ret
endfunction

function! emmet#getFileType(...) abort
  let flg = get(a:000, 0, 0)
  let type = ''

  if has_key(s:emmet_settings, &filetype)
    let types = [&filetype]
  else
    let types = split(&filetype, '\.')
  endif
  for part in types
    if emmet#lang#exists(part)
      let type = part
      break
    endif
    let base = emmet#getBaseType(part)
    if base !=# ''
      if flg
        let type = &filetype
      else
        let type = base
      endif
      unlet base
      break
    endif
  endfor
  if type ==# 'html'
    let pos = emmet#util#getcurpos()
    let type = synIDattr(synID(pos[1], pos[2], 1), 'name')
    if type =~# '^css\w'
      let type = 'css'
    endif
    if type =~# '^html\w'
      let type = 'html'
    endif
    if type =~# '^javaScript'
      let type = 'javascript'
    endif
    if len(type) ==# 0 && type =~# '^xml'
      let type = 'xml'
    endif
  endif
  if len(type) ==# 0 | let type = 'html' | endif
  return type
endfunction

function! emmet#getDollarExprs(expand) abort
  let expand = a:expand
  let dollar_list = []
  let dollar_reg = '\%(\\\)\@<!\${\(\([^{}]\|\%(\\\)\@\<=[{}]\)\{}\)}'
  while 1
    let matcharr = matchlist(expand, dollar_reg)
    if len(matcharr) > 0
      let key = get(matcharr, 1)
      if key !~# '^\d\+:'
        let key = substitute(key, '\\{', '{', 'g')
        let key = substitute(key, '\\}', '}', 'g')
        let value = emmet#getDollarValueByKey(key)
        if type(value) ==# type('')
          let expr = get(matcharr, 0)
          call add(dollar_list, {'expr': expr, 'value': value})
        endif
      endif
    else
      break
    endif
    let expand = substitute(expand, dollar_reg, '', '')
  endwhile
  return dollar_list
endfunction

function! emmet#getDollarValueByKey(key) abort
  let ret = 0
  let key = a:key
  let ftsetting = get(s:emmet_settings, emmet#getFileType())
  if type(ftsetting) ==# 4 && has_key(ftsetting, key)
    let V = get(ftsetting, key)
    if type(V) ==# 1 | return V | endif
  endif
  if type(ret) !=# 1 && has_key(s:emmet_settings.variables, key)
    let V = get(s:emmet_settings.variables, key)
    if type(V) ==# 1 | return V | endif
  endif
  if has_key(s:emmet_settings, 'custom_expands') && type(s:emmet_settings['custom_expands']) ==# 4
    for k in keys(s:emmet_settings['custom_expands'])
      if key =~# k
        let V = get(s:emmet_settings['custom_expands'], k)
        if type(V) ==# 1 | return V | endif
        if type(V) ==# 2 | return V(key) | endif
      endif
    endfor
  endif
  return ret
endfunction

function! emmet#reExpandDollarExpr(expand, times) abort
  let expand = a:expand
  let dollar_exprs = emmet#getDollarExprs(expand)
  if len(dollar_exprs) > 0
    if a:times < 9
      for n in range(len(dollar_exprs))
        let pair = get(dollar_exprs, n)
        let pat = get(pair, 'expr')
        let sub = get(pair, 'value')
        let expand = substitute(expand, pat, sub, '')
      endfor
      return emmet#reExpandDollarExpr(expand, a:times + 1)
    endif
  endif
  return expand
endfunction

function! emmet#expandDollarExpr(expand) abort
  return emmet#reExpandDollarExpr(a:expand, 0)
endfunction

function! emmet#expandCursorExpr(expand, mode) abort
  let expand = a:expand
  if expand !~# '\${cursor}'
    if a:mode ==# 2
      let expand = '${cursor}' . expand
    else
      let expand .= '${cursor}'
    endif
  endif
  let expand = substitute(expand, '\${\d\+:\?\([^}]\+\)}', '$select$$cursor$\1$select$', 'g')
  let expand = substitute(expand, '\${\d\+}', '$select$$cursor$$select$', 'g')
  let expand = substitute(expand, '\${cursor}', '$cursor$', '')
  let expand = substitute(expand, '\${cursor}', '', 'g')
  let expand = substitute(expand, '\${cursor}', '', 'g')
  return expand
endfunction

function! emmet#unescapeDollarExpr(expand) abort
  return substitute(a:expand, '\\\$', '$', 'g')
endfunction

function! emmet#expandAbbr(mode, abbr) range abort
  let type = emmet#getFileType()
  let rtype = emmet#getFileType(1)
  let indent = emmet#getIndentation(type)
  let expand = ''
  let line = ''
  let part = ''
  let rest = ''

  let filters = emmet#getFilters(type)
  if len(filters) ==# 0
    let filters = ['html']
  endif

  if a:mode ==# 2
    let leader = substitute(input('Tag: ', ''), '^\s*\(.*\)\s*$', '\1', 'g')
    if len(leader) ==# 0
      return ''
    endif
    if leader =~# s:filtermx
      let filters = map(split(matchstr(leader, s:filtermx)[1:], '\s*[^\\]\zs,\s*'), 'substitute(v:val, "\\\\\\\\zs.\\\\ze", "&", "g")')
      let leader = substitute(leader, s:filtermx, '', '')
    endif
    if leader =~# '\*'
      let query = substitute(leader, '*', '*' . (a:lastline - a:firstline + 1), '')
      if query !~# '}\s*$' && query !~# '\$#'
        let query .= '>{$#}'
      endif
      if emmet#useFilter(filters, '/')
        let spl = emmet#splitFilterArg(filters)
        let fline = getline(a:firstline)
        let query = substitute(query, '>\{0,1}{\$#}\s*$', '{\\$column\\$}*' . len(split(fline, spl)), '')
      else
        let spl = ''
      endif
      let items = emmet#parseIntoTree(query, type).child
      let itemno = 0
      for item in items
        let inner = emmet#toString(item, rtype, 0, filters, 0, indent)
        let inner = substitute(inner, '\$#', '$line'.(itemno*(a:lastline - a:firstline + 1)/len(items)+1).'$', 'g')
        let expand .= inner
        let itemno = itemno + 1
      endfor
      if emmet#useFilter(filters, 'e')
        let expand = substitute(expand, '&', '\&amp;', 'g')
        let expand = substitute(expand, '<', '\&lt;', 'g')
        let expand = substitute(expand, '>', '\&gt;', 'g')
      endif
      let line = getline(a:firstline)
      let part = substitute(line, '^\s*', '', '')
      for n in range(a:firstline, a:lastline)
        let lline = getline(n)
        let lpart = substitute(lline, '^\s\+', '', '')
        if emmet#useFilter(filters, 't')
          let lpart = substitute(lpart, '^[0-9.-]\+\s\+', '', '')
          let lpart = substitute(lpart, '\s\+$', '', '')
        endif
        if emmet#useFilter(filters, '/')
          for column in split(lpart, spl)
            let expand = substitute(expand, '\$column\$', '\=column', '')
          endfor
        else
          let expand = substitute(expand, '\$line'.(n-a:firstline+1).'\$', '\=lpart', 'g')
        endif
      endfor
      let expand = substitute(expand, '\$line\d*\$', '', 'g')
      let expand = substitute(expand, '\$column\$', '', 'g')
      let content = join(getline(a:firstline, a:lastline), "\n")
      if stridx(expand, '$#') < len(expand)-2
        let expand = substitute(expand, '^\(.*\)\$#\s*$', '\1', '')
      endif
      let expand = substitute(expand, '\$#', '\=content', 'g')
    else
      let str = ''
      if visualmode() ==# 'V'
        let line = getline(a:firstline)
        let lspaces = matchstr(line, '^\s*', '', '')
        let part = substitute(line, '^\s*', '', '')
        for n in range(a:firstline, a:lastline)
          if len(leader) > 0
            let line = getline(a:firstline)
            let spaces = matchstr(line, '^\s*', '', '')
            if len(spaces) >= len(lspaces)
              let str .= indent . getline(n)[len(lspaces):] . "\n"
            else
              let str .= getline(n) . "\n"
            endif
          else
            let lpart = substitute(getline(n), '^\s*', '', '')
            let str .= lpart . "\n"
          endif
        endfor
        if stridx(leader, '{$#}') ==# -1
          let leader .= '{$#}'
        endif
        let items = emmet#parseIntoTree(leader, type).child
      else
        let save_regcont = @"
        let save_regtype = getregtype('"')
        silent! normal! gvygv
        let str = @"
        call setreg('"', save_regcont, save_regtype)
        if stridx(leader, '{$#}') ==# -1
          let leader .= '{$#}'
        endif
        let items = emmet#parseIntoTree(leader, type).child
      endif
      for item in items
        let expand .= emmet#toString(item, rtype, 0, filters, 0, '')
      endfor
      if emmet#useFilter(filters, 'e')
        let expand = substitute(expand, '&', '\&amp;', 'g')
        let expand = substitute(expand, '<', '\&lt;', 'g')
        let expand = substitute(expand, '>', '\&gt;', 'g')
      endif
      if stridx(leader, '{$#}') !=# -1
        let expand = substitute(expand, '\$#', '\="\n" . str', 'g')
      endif
    endif
  elseif a:mode ==# 4
    let line = getline('.')
    let spaces = matchstr(line, '^\s*')
    if line !~# '^\s*$'
      put =spaces.a:abbr
    else
      call setline('.', spaces.a:abbr)
    endif
    normal! $
    call emmet#expandAbbr(0, '')
    return ''
  else
    let line = getline('.')
    if col('.') < len(line)
      let line = matchstr(line, '^\(.*\%'.col('.').'c\)')
    endif
    if a:mode ==# 1
      let part = matchstr(line, '\([a-zA-Z0-9:_\-\@|]\+\)$')
    else
      let part = matchstr(line, '\(\S.*\)$')
      let ftype = emmet#lang#exists(type) ? type : 'html'
      let part = emmet#lang#{ftype}#findTokens(part)
      let line = line[0: strridx(line, part) + len(part) - 1]
    endif
    if col('.') ==# col('$')
      let rest = ''
    else
      let rest = getline('.')[len(line):]
    endif
    let str = part
    if str =~# s:filtermx
      let filters = split(matchstr(str, s:filtermx)[1:], '\s*,\s*')
      let str = substitute(str, s:filtermx, '', '')
    endif
    let items = emmet#parseIntoTree(str, rtype).child
    for item in items
      let expand .= emmet#toString(item, rtype, 0, filters, 0, indent)
    endfor
    if emmet#useFilter(filters, 'e')
      let expand = substitute(expand, '&', '\&amp;', 'g')
      let expand = substitute(expand, '<', '\&lt;', 'g')
      let expand = substitute(expand, '>', '\&gt;', 'g')
    endif
    let expand = substitute(expand, '\$line\([0-9]\+\)\$', '\=submatch(1)', 'g')
  endif
  let expand = emmet#expandDollarExpr(expand)
  let expand = emmet#expandCursorExpr(expand, a:mode)
  if len(expand)
    if has_key(s:emmet_settings, 'timezone') && len(s:emmet_settings.timezone)
      let expand = substitute(expand, '${datetime}', strftime('%Y-%m-%dT%H:%M:%S') . s:emmet_settings.timezone, 'g')
    else
      " TODO: on windows, %z/%Z is 'Tokyo(Standard)'
      let expand = substitute(expand, '${datetime}', strftime('%Y-%m-%dT%H:%M:%S %z'), 'g')
    endif
    let expand = emmet#unescapeDollarExpr(expand)
    if a:mode ==# 2 && visualmode() ==# 'v'
      if a:firstline ==# a:lastline
        let expand = substitute(expand, '[\r\n]\s*', '', 'g')
      else
        let expand = substitute(expand, '[\n]$', '', 'g')
      endif
      silent! normal! gv
      let col = col('''<')
      silent! normal! c
      let line = getline('.')
      let lhs = matchstr(line, '.*\%<'.col.'c.')
      let rhs = matchstr(line, '\%>'.(col-1).'c.*')
      let expand = lhs.expand.rhs
      let lines = split(expand, '\n')
      call setline(line('.'), lines[0])
      if len(lines) > 1
        call append(line('.'), lines[1:])
      endif
    else
      if line[:-len(part)-1] =~# '^\s\+$'
        let indent = line[:-len(part)-1]
      else
        let indent = ''
      endif
      let expand = substitute(expand, '[\r\n]\s*$', '', 'g')
      if emmet#useFilter(filters, 's')
        let epart = substitute(expand, '[\r\n]\s*', '', 'g')
      else
        let epart = substitute(expand, '[\r\n]', "\n" . indent, 'g')
      endif
      let expand = line[:-len(part)-1] . epart . rest
      let lines = split(expand, '[\r\n]', 1)
      if a:mode ==# 2
        silent! exe 'normal! gvc'
      endif
      call setline('.', lines[0])
      if len(lines) > 1
        call append('.', lines[1:])
      endif
    endif
  endif
  if g:emmet_debug > 1
    call getchar()
  endif
  if search('\ze\$\(cursor\|select\)\$', 'c')
    let oldselection = &selection
    let &selection = 'inclusive'
    if foldclosed(line('.')) !=# -1
      silent! foldopen
    endif
    let pos = emmet#util#getcurpos()
    let use_selection = emmet#getResource(type, 'use_selection', 0)
    try
      let l:gdefault = &gdefault
      let &gdefault = 0
      if use_selection && getline('.')[col('.')-1:] =~# '^\$select'
        let pos[2] += 1
        silent! s/\$select\$//
        let next = searchpos('.\ze\$select\$', 'nW')
        silent! %s/\$\(cursor\|select\)\$//g
        call emmet#util#selectRegion([pos[1:2], next])
        return "\<esc>gv"
      else
        silent! %s/\$\(cursor\|select\)\$//g
        silent! call setpos('.', pos)
        if col('.') < col('$')
          return "\<right>"
        endif
      endif
    finally
      let &gdefault = l:gdefault
    endtry
    let &selection = oldselection
  endif
  return ''
endfunction

function! emmet#updateTag() abort
  let type = emmet#getFileType()
  let region = emmet#util#searchRegion('<\S', '>')
  if !emmet#util#regionIsValid(region) || !emmet#util#cursorInRegion(region)
    return ''
  endif
  let content = emmet#util#getContent(region)
  let content = matchstr(content,  '^<[^><]\+>')
  if content !~# '^<[^><]\+>$'
    return ''
  endif
  let current = emmet#lang#html#parseTag(content)
  if empty(current)
    return ''
  endif

  let str = substitute(input('Enter Abbreviation: ', ''), '^\s*\(.*\)\s*$', '\1', 'g')
  let item = emmet#parseIntoTree(str, type).child[0]
  for k in keys(item.attr)
    let current.attr[k] = item.attr[k]
  endfor
  let html = substitute(emmet#toString(current, 'html', 1), '\n', '', '')
  let html = substitute(html, '\${cursor}', '', '')
  let html = matchstr(html,  '^<[^><]\+>')
  call emmet#util#setContent(region, html)
  return ''
endfunction

function! emmet#moveNextPrevItem(flag) abort
  let type = emmet#getFileType()
  let rtype = emmet#lang#exists(type) ? type : 'html'
  return emmet#lang#{rtype}#moveNextPrevItem(a:flag)
endfunction

function! emmet#moveNextPrev(flag) abort
  let type = emmet#getFileType()
  let rtype = emmet#lang#exists(type) ? type : 'html'
  return emmet#lang#{rtype}#moveNextPrev(a:flag)
endfunction

function! emmet#imageSize() abort
  let orgpos = emmet#util#getcurpos()
  let type = emmet#getFileType()
  let rtype = emmet#lang#exists(type) ? type : 'html'
  call emmet#lang#{rtype}#imageSize()
  silent! call setpos('.', orgpos)
  return ''
endfunction

function! emmet#encodeImage() abort
  let type = emmet#getFileType()
  let rtype = emmet#lang#exists(type) ? type : 'html'
  return emmet#lang#{rtype}#encodeImage()
endfunction

function! emmet#toggleComment() abort
  let type = emmet#getFileType()
  let rtype = emmet#lang#exists(type) ? type : 'html'
  call emmet#lang#{rtype}#toggleComment()
  return ''
endfunction

function! emmet#balanceTag(flag) range abort
  let type = emmet#getFileType()
  let rtype = emmet#lang#exists(type) ? type : 'html'
  return emmet#lang#{rtype}#balanceTag(a:flag)
endfunction

function! emmet#splitJoinTag() abort
  let type = emmet#getFileType()
  let rtype = emmet#lang#exists(type) ? type : 'html'
  return emmet#lang#{rtype}#splitJoinTag()
endfunction

function! emmet#mergeLines() range abort
  let lines = join(map(getline(a:firstline, a:lastline), 'matchstr(v:val, "^\\s*\\zs.*\\ze\\s*$")'), '')
  let indent = substitute(getline('.'), '^\(\s*\).*', '\1', '')
  silent! exe 'normal! gvc'
  call setline('.', indent . lines)
endfunction

function! emmet#removeTag() abort
  let type = emmet#getFileType()
  let rtype = emmet#lang#exists(type) ? type : 'html'
  call emmet#lang#{rtype}#removeTag()
  return ''
endfunction

function! emmet#anchorizeURL(flag) abort
  let mx = 'https\=:\/\/[-!#$%&*+,./:;=?@0-9a-zA-Z_~]\+'
  let pos1 = searchpos(mx, 'bcnW')
  let url = matchstr(getline(pos1[0])[pos1[1]-1:], mx)
  let block = [pos1, [pos1[0], pos1[1] + len(url) - 1]]
  if !emmet#util#cursorInRegion(block)
    return ''
  endif

  let mx = '.*<title[^>]*>\s*\zs\([^<]\+\)\ze\s*<\/title[^>]*>.*'
  let content = emmet#util#getContentFromURL(url)
  let content = substitute(content, '\r', '', 'g')
  let content = substitute(content, '[ \n]\+', ' ', 'g')
  let content = substitute(content, '<!--.\{-}-->', '', 'g')
  let title = matchstr(content, mx)

  let type = emmet#getFileType()
  let rtype = emmet#lang#exists(type) ? type : 'html'
  if &filetype ==# 'markdown'
    let expand = printf('[%s](%s)', substitute(title, '[\[\]]', '\\&', 'g'), url)
  elseif a:flag ==# 0
    let a = emmet#lang#html#parseTag('<a>')
    let a.attr.href = url
    let a.value = '{' . title . '}'
    let expand = emmet#toString(a, rtype, 0, [])
    let expand = substitute(expand, '\${cursor}', '', 'g')
  else
    let body = emmet#util#getTextFromHTML(content)
    let body = '{' . substitute(body, '^\(.\{0,100}\).*', '\1', '') . '...}'

    let blockquote = emmet#lang#html#parseTag('<blockquote class="quote">')
    let a = emmet#lang#html#parseTag('<a>')
    let a.attr.href = url
    let a.value = '{' . title . '}'
    call add(blockquote.child, a)
    call add(blockquote.child, emmet#lang#html#parseTag('<br/>'))
    let p = emmet#lang#html#parseTag('<p>')
    let p.value = body
    call add(blockquote.child, p)
    let cite = emmet#lang#html#parseTag('<cite>')
    let cite.value = '{' . url . '}'
    call add(blockquote.child, cite)
    let expand = emmet#toString(blockquote, rtype, 0, [])
    let expand = substitute(expand, '\${cursor}', '', 'g')
  endif
  let indent = substitute(getline('.'), '^\(\s*\).*', '\1', '')
  let expand = substitute(expand, "\n", "\n" . indent, 'g')
  call emmet#util#setContent(block, expand)
  return ''
endfunction

function! emmet#codePretty() range abort
  let type = input('FileType: ', &filetype, 'filetype')
  if len(type) ==# 0
    return
  endif
  let block = emmet#util#getVisualBlock()
  let content = emmet#util#getContent(block)
  silent! 1new
  let &l:filetype = type
  call setline(1, split(content, "\n"))
  let old_lazyredraw = &lazyredraw
  set lazyredraw
  silent! TOhtml
  let &lazyredraw = old_lazyredraw
  let content = join(getline(1, '$'), "\n")
  silent! bw!
  silent! bw!
  let content = matchstr(content, '<body[^>]*>[\s\n]*\zs.*\ze</body>')
  call emmet#util#setContent(block, content)
endfunction

function! emmet#expandWord(abbr, type, orig) abort
  let str = a:abbr
  let type = a:type
  let indent = emmet#getIndentation(type)

  if len(type) ==# 0 | let type = 'html' | endif
  if str =~# s:filtermx
    let filters = split(matchstr(str, s:filtermx)[1:], '\s*,\s*')
    let str = substitute(str, s:filtermx, '', '')
  else
    let filters = emmet#getFilters(a:type)
    if len(filters) ==# 0
      let filters = ['html']
    endif
  endif
  let str = substitute(str, '|', '${cursor}', 'g')
  let items = emmet#parseIntoTree(str, a:type).child
  let expand = ''
  for item in items
    let expand .= emmet#toString(item, a:type, 0, filters, 0, indent)
  endfor
  if emmet#useFilter(filters, 'e')
    let expand = substitute(expand, '&', '\&amp;', 'g')
    let expand = substitute(expand, '<', '\&lt;', 'g')
    let expand = substitute(expand, '>', '\&gt;', 'g')
  endif
  if emmet#useFilter(filters, 's')
    let expand = substitute(expand, "\n\s\*", '', 'g')
  endif
  if a:orig ==# 0
    let expand = emmet#expandDollarExpr(expand)
    let expand = substitute(expand, '\${cursor}', '', 'g')
  endif
  return expand
endfunction

function! emmet#getSnippets(type) abort
  let type = a:type
  if len(type) ==# 0 || !has_key(s:emmet_settings, type)
    let type = 'html'
  endif
  return emmet#getResource(type, 'snippets', {})
endfunction

function! emmet#completeTag(findstart, base) abort
  if a:findstart
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && line[start - 1] =~# '[a-zA-Z0-9:_\@\-]'
      let start -= 1
    endwhile
    return start
  else
    let type = emmet#getFileType()
    let res = []

    let snippets = emmet#getResource(type, 'snippets', {})
    for item in keys(snippets)
      if stridx(item, a:base) !=# -1
        call add(res, substitute(item, '\${cursor}\||', '', 'g'))
      endif
    endfor
    let aliases = emmet#getResource(type, 'aliases', {})
    for item in values(aliases)
      if stridx(item, a:base) !=# -1
        call add(res, substitute(item, '\${cursor}\||', '', 'g'))
      endif
    endfor
    return res
  endif
endfunction

unlet! s:emmet_settings
let s:emmet_settings = {
\    'variables': {
\      'lang': "en",
\      'locale': "en-US",
\      'charset': "UTF-8",
\      'newline': "\n",
\      'use_selection': 0,
\    },
\    'custom_expands' : {
\      '^\%(lorem\|lipsum\)\(\d*\)$' : function('emmet#lorem#en#expand'),
\    },
\    'css': {
\        'snippets': {
\           "@i": "@import url(|);",
\           "@import": "@import url(|);",
\           "@m": "@media ${1:screen} {\n\t|\n}",
\           "@media": "@media ${1:screen} {\n\t|\n}",
\           "@f": "@font-face {\n\tfont-family:|;\n\tsrc:url(|);\n}",
\           "@f+": "@font-face {\n\tfont-family: '${1:FontName}';\n\tsrc: url('${2:FileName}.eot');\n\tsrc: url('${2:FileName}.eot?#iefix') format('embedded-opentype'),\n\t\t url('${2:FileName}.woff') format('woff'),\n\t\t url('${2:FileName}.ttf') format('truetype'),\n\t\t url('${2:FileName}.svg#${1:FontName}') format('svg');\n\tfont-style: ${3:normal};\n\tfont-weight: ${4:normal};\n}",
\           "@kf": "@-webkit-keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}\n@-o-keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}\n@-moz-keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}\n@keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}",
\           "anim": "animation:|;",
\           "anim-": "animation:${1:name} ${2:duration} ${3:timing-function} ${4:delay} ${5:iteration-count} ${6:direction} ${7:fill-mode};",
\           "animdel": "animation-delay:${1:time};",
\           "animdir": "animation-direction:${1:normal};",
\           "animdir:n": "animation-direction:normal;",
\           "animdir:r": "animation-direction:reverse;",
\           "animdir:a": "animation-direction:alternate;",
\           "animdir:ar": "animation-direction:alternate-reverse;",
\           "animdur": "animation-duration:${1:0}s;",
\           "animfm": "animation-fill-mode:${1:both};",
\           "animfm:f": "animation-fill-mode:forwards;",
\           "animfm:b": "animation-fill-mode:backwards;",
\           "animfm:bt": "animation-fill-mode:both;",
\           "animfm:bh": "animation-fill-mode:both;",
\           "animic": "animation-iteration-count:${1:1};",
\           "animic:i": "animation-iteration-count:infinite;",
\           "animn": "animation-name:${1:none};",
\           "animps": "animation-play-state:${1:running};",
\           "animps:p": "animation-play-state:paused;",
\           "animps:r": "animation-play-state:running;",
\           "animtf": "animation-timing-function:${1:linear};",
\           "animtf:e": "animation-timing-function:ease;",
\           "animtf:ei": "animation-timing-function:ease-in;",
\           "animtf:eo": "animation-timing-function:ease-out;",
\           "animtf:eio": "animation-timing-function:ease-in-out;",
\           "animtf:l": "animation-timing-function:linear;",
\           "animtf:cb": "animation-timing-function:cubic-bezier(${1:0.1}, ${2:0.7}, ${3:1.0}, ${3:0.1});",
\           "ap": "appearance:${none};",
\           "!": "!important",
\           "pos": "position:${1:relative};",
\           "pos:s": "position:static;",
\           "pos:a": "position:absolute;",
\           "pos:r": "position:relative;",
\           "pos:f": "position:fixed;",
\           "t": "top:|;",
\           "t:a": "top:auto;",
\           "r": "right:|;",
\           "r:a": "right:auto;",
\           "b": "bottom:|;",
\           "b:a": "bottom:auto;",
\           "l": "left:|;",
\           "l:a": "left:auto;",
\           "z": "z-index:|;",
\           "z:a": "z-index:auto;",
\           "fl": "float:${1:left};",
\           "fl:n": "float:none;",
\           "fl:l": "float:left;",
\           "fl:r": "float:right;",
\           "cl": "clear:${1:both};",
\           "cl:n": "clear:none;",
\           "cl:l": "clear:left;",
\           "cl:r": "clear:right;",
\           "cl:b": "clear:both;",
\           "colm": "columns:|;",
\           "colmc": "column-count:|;",
\           "colmf": "column-fill:|;",
\           "colmg": "column-gap:|;",
\           "colmr": "column-rule:|;",
\           "colmrc": "column-rule-color:|;",
\           "colmrs": "column-rule-style:|;",
\           "colmrw": "column-rule-width:|;",
\           "colms": "column-span:|;",
\           "colmw": "column-width:|;",
\           "d": "display:${1:block};",
\           "d:n": "display:none;",
\           "d:b": "display:block;",
\           "d:f": "display:flex;",
\           "d:i": "display:inline;",
\           "d:ib": "display:inline-block;",
\           "d:ib+": "display: inline-block;\n*display: inline;\n*zoom: 1;",
\           "d:li": "display:list-item;",
\           "d:ri": "display:run-in;",
\           "d:cp": "display:compact;",
\           "d:tb": "display:table;",
\           "d:itb": "display:inline-table;",
\           "d:tbcp": "display:table-caption;",
\           "d:tbcl": "display:table-column;",
\           "d:tbclg": "display:table-column-group;",
\           "d:tbhg": "display:table-header-group;",
\           "d:tbfg": "display:table-footer-group;",
\           "d:tbr": "display:table-row;",
\           "d:tbrg": "display:table-row-group;",
\           "d:tbc": "display:table-cell;",
\           "d:rb": "display:ruby;",
\           "d:rbb": "display:ruby-base;",
\           "d:rbbg": "display:ruby-base-group;",
\           "d:rbt": "display:ruby-text;",
\           "d:rbtg": "display:ruby-text-group;",
\           "v": "visibility:${1:hidden};",
\           "v:v": "visibility:visible;",
\           "v:h": "visibility:hidden;",
\           "v:c": "visibility:collapse;",
\           "ov": "overflow:${1:hidden};",
\           "ov:v": "overflow:visible;",
\           "ov:h": "overflow:hidden;",
\           "ov:s": "overflow:scroll;",
\           "ov:a": "overflow:auto;",
\           "ovx": "overflow-x:${1:hidden};",
\           "ovx:v": "overflow-x:visible;",
\           "ovx:h": "overflow-x:hidden;",
\           "ovx:s": "overflow-x:scroll;",
\           "ovx:a": "overflow-x:auto;",
\           "ovy": "overflow-y:${1:hidden};",
\           "ovy:v": "overflow-y:visible;",
\           "ovy:h": "overflow-y:hidden;",
\           "ovy:s": "overflow-y:scroll;",
\           "ovy:a": "overflow-y:auto;",
\           "ovs": "overflow-style:${1:scrollbar};",
\           "ovs:a": "overflow-style:auto;",
\           "ovs:s": "overflow-style:scrollbar;",
\           "ovs:p": "overflow-style:panner;",
\           "ovs:m": "overflow-style:move;",
\           "ovs:mq": "overflow-style:marquee;",
\           "zoo": "zoom:1;",
\           "zm": "zoom:1;",
\           "cp": "clip:|;",
\           "cp:a": "clip:auto;",
\           "cp:r": "clip:rect(${1:top} ${2:right} ${3:bottom} ${4:left});",
\           "bxz": "box-sizing:${1:border-box};",
\           "bxz:cb": "box-sizing:content-box;",
\           "bxz:bb": "box-sizing:border-box;",
\           "bxsh": "box-shadow:${1:inset }${2:hoff} ${3:voff} ${4:blur} ${5:color};",
\           "bxsh:r": "box-shadow:${1:inset }${2:hoff} ${3:voff} ${4:blur} ${5:spread }rgb(${6:0}, ${7:0}, ${8:0});",
\           "bxsh:ra": "box-shadow:${1:inset }${2:h} ${3:v} ${4:blur} ${5:spread }rgba(${6:0}, ${7:0}, ${8:0}, .${9:5});",
\           "bxsh:n": "box-shadow:none;",
\           "m": "margin:|;",
\           "m:a": "margin:auto;",
\           "mt": "margin-top:|;",
\           "mt:a": "margin-top:auto;",
\           "mr": "margin-right:|;",
\           "mr:a": "margin-right:auto;",
\           "mb": "margin-bottom:|;",
\           "mb:a": "margin-bottom:auto;",
\           "ml": "margin-left:|;",
\           "ml:a": "margin-left:auto;",
\           "p": "padding:|;",
\           "pt": "padding-top:|;",
\           "pr": "padding-right:|;",
\           "pb": "padding-bottom:|;",
\           "pl": "padding-left:|;",
\           "w": "width:|;",
\           "w:a": "width:auto;",
\           "h": "height:|;",
\           "h:a": "height:auto;",
\           "maw": "max-width:|;",
\           "maw:n": "max-width:none;",
\           "mah": "max-height:|;",
\           "mah:n": "max-height:none;",
\           "miw": "min-width:|;",
\           "mih": "min-height:|;",
\           "mar": "max-resolution:${1:res};",
\           "mir": "min-resolution:${1:res};",
\           "ori": "orientation:|;",
\           "ori:l": "orientation:landscape;",
\           "ori:p": "orientation:portrait;",
\           "ol": "outline:|;",
\           "ol:n": "outline:none;",
\           "olo": "outline-offset:|;",
\           "olw": "outline-width:|;",
\           "olw:tn": "outline-width:thin;",
\           "olw:m": "outline-width:medium;",
\           "olw:tc": "outline-width:thick;",
\           "ols": "outline-style:|;",
\           "ols:n": "outline-style:none;",
\           "ols:dt": "outline-style:dotted;",
\           "ols:ds": "outline-style:dashed;",
\           "ols:s": "outline-style:solid;",
\           "ols:db": "outline-style:double;",
\           "ols:g": "outline-style:groove;",
\           "ols:r": "outline-style:ridge;",
\           "ols:i": "outline-style:inset;",
\           "ols:o": "outline-style:outset;",
\           "olc": "outline-color:#${1:000};",
\           "olc:i": "outline-color:invert;",
\           "bfv": "backface-visibility:|;",
\           "bfv:h": "backface-visibility:hidden;",
\           "bfv:v": "backface-visibility:visible;",
\           "bd": "border:|;",
\           "bd+": "border:${1:1px} ${2:solid} ${3:#000};",
\           "bd:n": "border:none;",
\           "bdbk": "border-break:${1:close};",
\           "bdbk:c": "border-break:close;",
\           "bdcl": "border-collapse:|;",
\           "bdcl:c": "border-collapse:collapse;",
\           "bdcl:s": "border-collapse:separate;",
\           "bdc": "border-color:#${1:000};",
\           "bdc:t": "border-color:transparent;",
\           "bdi": "border-image:url(|);",
\           "bdi:n": "border-image:none;",
\           "bdti": "border-top-image:url(|);",
\           "bdti:n": "border-top-image:none;",
\           "bdri": "border-right-image:url(|);",
\           "bdri:n": "border-right-image:none;",
\           "bdbi": "border-bottom-image:url(|);",
\           "bdbi:n": "border-bottom-image:none;",
\           "bdli": "border-left-image:url(|);",
\           "bdli:n": "border-left-image:none;",
\           "bdci": "border-corner-image:url(|);",
\           "bdci:n": "border-corner-image:none;",
\           "bdci:c": "border-corner-image:continue;",
\           "bdtli": "border-top-left-image:url(|);",
\           "bdtli:n": "border-top-left-image:none;",
\           "bdtli:c": "border-top-left-image:continue;",
\           "bdtri": "border-top-right-image:url(|);",
\           "bdtri:n": "border-top-right-image:none;",
\           "bdtri:c": "border-top-right-image:continue;",
\           "bdbri": "border-bottom-right-image:url(|);",
\           "bdbri:n": "border-bottom-right-image:none;",
\           "bdbri:c": "border-bottom-right-image:continue;",
\           "bdbli": "border-bottom-left-image:url(|);",
\           "bdbli:n": "border-bottom-left-image:none;",
\           "bdbli:c": "border-bottom-left-image:continue;",
\           "bdf": "border-fit:${1:repeat};",
\           "bdf:c": "border-fit:clip;",
\           "bdf:r": "border-fit:repeat;",
\           "bdf:sc": "border-fit:scale;",
\           "bdf:st": "border-fit:stretch;",
\           "bdf:ow": "border-fit:overwrite;",
\           "bdf:of": "border-fit:overflow;",
\           "bdf:sp": "border-fit:space;",
\           "bdlen": "border-length:|;",
\           "bdlen:a": "border-length:auto;",
\           "bdsp": "border-spacing:|;",
\           "bds": "border-style:|;",
\           "bds:n": "border-style:none;",
\           "bds:h": "border-style:hidden;",
\           "bds:dt": "border-style:dotted;",
\           "bds:ds": "border-style:dashed;",
\           "bds:s": "border-style:solid;",
\           "bds:db": "border-style:double;",
\           "bds:dtds": "border-style:dot-dash;",
\           "bds:dtdtds": "border-style:dot-dot-dash;",
\           "bds:w": "border-style:wave;",
\           "bds:g": "border-style:groove;",
\           "bds:r": "border-style:ridge;",
\           "bds:i": "border-style:inset;",
\           "bds:o": "border-style:outset;",
\           "bdw": "border-width:|;",
\           "bdtw": "border-top-width:|;",
\           "bdrw": "border-right-width:|;",
\           "bdbw": "border-bottom-width:|;",
\           "bdlw": "border-left-width:|;",
\           "bdt": "border-top:|;",
\           "bt": "border-top:|;",
\           "bdt+": "border-top:${1:1px} ${2:solid} ${3:#000};",
\           "bdt:n": "border-top:none;",
\           "bdts": "border-top-style:|;",
\           "bdts:n": "border-top-style:none;",
\           "bdtc": "border-top-color:#${1:000};",
\           "bdtc:t": "border-top-color:transparent;",
\           "bdr": "border-right:|;",
\           "br": "border-right:|;",
\           "bdr+": "border-right:${1:1px} ${2:solid} ${3:#000};",
\           "bdr:n": "border-right:none;",
\           "bdrst": "border-right-style:|;",
\           "bdrst:n": "border-right-style:none;",
\           "bdrc": "border-right-color:#${1:000};",
\           "bdrc:t": "border-right-color:transparent;",
\           "bdb": "border-bottom:|;",
\           "bb": "border-bottom:|;",
\           "bdb+": "border-bottom:${1:1px} ${2:solid} ${3:#000};",
\           "bdb:n": "border-bottom:none;",
\           "bdbs": "border-bottom-style:|;",
\           "bdbs:n": "border-bottom-style:none;",
\           "bdbc": "border-bottom-color:#${1:000};",
\           "bdbc:t": "border-bottom-color:transparent;",
\           "bdl": "border-left:|;",
\           "bl": "border-left:|;",
\           "bdl+": "border-left:${1:1px} ${2:solid} ${3:#000};",
\           "bdl:n": "border-left:none;",
\           "bdls": "border-left-style:|;",
\           "bdls:n": "border-left-style:none;",
\           "bdlc": "border-left-color:#${1:000};",
\           "bdlc:t": "border-left-color:transparent;",
\           "bdrs": "border-radius:|;",
\           "bdtrrs": "border-top-right-radius:|;",
\           "bdtlrs": "border-top-left-radius:|;",
\           "bdbrrs": "border-bottom-right-radius:|;",
\           "bdblrs": "border-bottom-left-radius:|;",
\           "bg": "background:#${1:000};",
\           "bg+": "background:${1:#fff} url(${2}) ${3:0} ${4:0} ${5:no-repeat};",
\           "bg:n": "background:none;",
\           "bg:ie": "filter:progid:DXImageTransform.Microsoft.AlphaImageLoader(src='${1:x}.png',sizingMethod='${2:crop}');",
\           "bgc": "background-color:#${1:fff};",
\           "bgc:t": "background-color:transparent;",
\           "bgi": "background-image:url(|);",
\           "bgi:n": "background-image:none;",
\           "bgr": "background-repeat:|;",
\           "bgr:n": "background-repeat:no-repeat;",
\           "bgr:x": "background-repeat:repeat-x;",
\           "bgr:y": "background-repeat:repeat-y;",
\           "bgr:sp": "background-repeat:space;",
\           "bgr:rd": "background-repeat:round;",
\           "bga": "background-attachment:|;",
\           "bga:f": "background-attachment:fixed;",
\           "bga:s": "background-attachment:scroll;",
\           "bgp": "background-position:${1:0} ${2:0};",
\           "bgpx": "background-position-x:|;",
\           "bgpy": "background-position-y:|;",
\           "bgbk": "background-break:|;",
\           "bgbk:bb": "background-break:bounding-box;",
\           "bgbk:eb": "background-break:each-box;",
\           "bgbk:c": "background-break:continuous;",
\           "bgcp": "background-clip:${1:padding-box};",
\           "bgcp:bb": "background-clip:border-box;",
\           "bgcp:pb": "background-clip:padding-box;",
\           "bgcp:cb": "background-clip:content-box;",
\           "bgcp:nc": "background-clip:no-clip;",
\           "bgo": "background-origin:|;",
\           "bgo:pb": "background-origin:padding-box;",
\           "bgo:bb": "background-origin:border-box;",
\           "bgo:cb": "background-origin:content-box;",
\           "bgsz": "background-size:|;",
\           "bgsz:a": "background-size:auto;",
\           "bgsz:ct": "background-size:contain;",
\           "bgsz:cv": "background-size:cover;",
\           "c": "color:#${1:000};",
\           "c:r": "color:rgb(${1:0}, ${2:0}, ${3:0});",
\           "c:ra": "color:rgba(${1:0}, ${2:0}, ${3:0}, .${4:5});",
\           "cm": "/* |${child} */",
\           "cnt": "content:'|';",
\           "cnt:n": "content:normal;",
\           "cnt:oq": "content:open-quote;",
\           "cnt:noq": "content:no-open-quote;",
\           "cnt:cq": "content:close-quote;",
\           "cnt:ncq": "content:no-close-quote;",
\           "cnt:a": "content:attr(|);",
\           "cnt:c": "content:counter(|);",
\           "cnt:cs": "content:counters(|);",
\           "tbl": "table-layout:|;",
\           "tbl:a": "table-layout:auto;",
\           "tbl:f": "table-layout:fixed;",
\           "cps": "caption-side:|;",
\           "cps:t": "caption-side:top;",
\           "cps:b": "caption-side:bottom;",
\           "ec": "empty-cells:|;",
\           "ec:s": "empty-cells:show;",
\           "ec:h": "empty-cells:hide;",
\           "lis": "list-style:|;",
\           "lis:n": "list-style:none;",
\           "lisp": "list-style-position:|;",
\           "lisp:i": "list-style-position:inside;",
\           "lisp:o": "list-style-position:outside;",
\           "list": "list-style-type:|;",
\           "list:n": "list-style-type:none;",
\           "list:d": "list-style-type:disc;",
\           "list:c": "list-style-type:circle;",
\           "list:s": "list-style-type:square;",
\           "list:dc": "list-style-type:decimal;",
\           "list:dclz": "list-style-type:decimal-leading-zero;",
\           "list:lr": "list-style-type:lower-roman;",
\           "list:ur": "list-style-type:upper-roman;",
\           "lisi": "list-style-image:|;",
\           "lisi:n": "list-style-image:none;",
\           "q": "quotes:|;",
\           "q:n": "quotes:none;",
\           "q:ru": "quotes:'\\00AB' '\\00BB' '\\201E' '\\201C';",
\           "q:en": "quotes:'\\201C' '\\201D' '\\2018' '\\2019';",
\           "ct": "content:|;",
\           "ct:n": "content:normal;",
\           "ct:oq": "content:open-quote;",
\           "ct:noq": "content:no-open-quote;",
\           "ct:cq": "content:close-quote;",
\           "ct:ncq": "content:no-close-quote;",
\           "ct:a": "content:attr(|);",
\           "ct:c": "content:counter(|);",
\           "ct:cs": "content:counters(|);",
\           "coi": "counter-increment:|;",
\           "cor": "counter-reset:|;",
\           "va": "vertical-align:${1:top};",
\           "va:sup": "vertical-align:super;",
\           "va:t": "vertical-align:top;",
\           "va:tt": "vertical-align:text-top;",
\           "va:m": "vertical-align:middle;",
\           "va:bl": "vertical-align:baseline;",
\           "va:b": "vertical-align:bottom;",
\           "va:tb": "vertical-align:text-bottom;",
\           "va:sub": "vertical-align:sub;",
\           "ta": "text-align:${1:left};",
\           "ta:l": "text-align:left;",
\           "ta:c": "text-align:center;",
\           "ta:r": "text-align:right;",
\           "ta:j": "text-align:justify;",
\           "ta-lst": "text-align-last:|;",
\           "tal:a": "text-align-last:auto;",
\           "tal:l": "text-align-last:left;",
\           "tal:c": "text-align-last:center;",
\           "tal:r": "text-align-last:right;",
\           "td": "text-decoration:${1:none};",
\           "td:n": "text-decoration:none;",
\           "td:u": "text-decoration:underline;",
\           "td:o": "text-decoration:overline;",
\           "td:l": "text-decoration:line-through;",
\           "te": "text-emphasis:|;",
\           "te:n": "text-emphasis:none;",
\           "te:ac": "text-emphasis:accent;",
\           "te:dt": "text-emphasis:dot;",
\           "te:c": "text-emphasis:circle;",
\           "te:ds": "text-emphasis:disc;",
\           "te:b": "text-emphasis:before;",
\           "te:a": "text-emphasis:after;",
\           "th": "text-height:|;",
\           "th:a": "text-height:auto;",
\           "th:f": "text-height:font-size;",
\           "th:t": "text-height:text-size;",
\           "th:m": "text-height:max-size;",
\           "ti": "text-indent:|;",
\           "ti:-": "text-indent:-9999px;",
\           "tj": "text-justify:|;",
\           "tj:a": "text-justify:auto;",
\           "tj:iw": "text-justify:inter-word;",
\           "tj:ii": "text-justify:inter-ideograph;",
\           "tj:ic": "text-justify:inter-cluster;",
\           "tj:d": "text-justify:distribute;",
\           "tj:k": "text-justify:kashida;",
\           "tj:t": "text-justify:tibetan;",
\           "tov": "text-overflow:${ellipsis};",
\           "tov:e": "text-overflow:ellipsis;",
\           "tov:c": "text-overflow:clip;",
\           "to": "text-outline:|;",
\           "to+": "text-outline:${1:0} ${2:0} ${3:#000};",
\           "to:n": "text-outline:none;",
\           "tr": "text-replace:|;",
\           "tr:n": "text-replace:none;",
\           "tt": "text-transform:${1:uppercase};",
\           "tt:n": "text-transform:none;",
\           "tt:c": "text-transform:capitalize;",
\           "tt:u": "text-transform:uppercase;",
\           "tt:l": "text-transform:lowercase;",
\           "tw": "text-wrap:|;",
\           "tw:n": "text-wrap:normal;",
\           "tw:no": "text-wrap:none;",
\           "tw:u": "text-wrap:unrestricted;",
\           "tw:s": "text-wrap:suppress;",
\           "tsh": "text-shadow:${1:hoff} ${2:voff} ${3:blur} ${4:#000};",
\           "tsh:r": "text-shadow:${1:h} ${2:v} ${3:blur} rgb(${4:0}, ${5:0}, ${6:0});",
\           "tsh:ra": "text-shadow:${1:h} ${2:v} ${3:blur} rgba(${4:0}, ${5:0}, ${6:0}, .${7:5});",
\           "tsh+": "text-shadow:${1:0} ${2:0} ${3:0} ${4:#000};",
\           "tsh:n": "text-shadow:none;",
\           "trf": "transform:|;",
\           "trf:skx": "transform: skewX(${1:angle});",
\           "trf:sky": "transform: skewY(${1:angle});",
\           "trf:sc": "transform: scale(${1:x}, ${2:y});",
\           "trf:scx": "transform: scaleX(${1:x});",
\           "trf:scy": "transform: scaleY(${1:y});",
\           "trf:scz": "transform: scaleZ(${1:z});",
\           "trf:sc3": "transform: scale3d(${1:x}, ${2:y}, ${3:z});",
\           "trf:r": "transform: rotate(${1:angle});",
\           "trf:rx": "transform: rotateX(${1:angle});",
\           "trf:ry": "transform: rotateY(${1:angle});",
\           "trf:rz": "transform: rotateZ(${1:angle});",
\           "trf:t": "transform: translate(${1:x}, ${2:y});",
\           "trf:tx": "transform: translateX(${1:x});",
\           "trf:ty": "transform: translateY(${1:y});",
\           "trf:tz": "transform: translateZ(${1:z});",
\           "trf:t3": "transform: translate3d(${1:tx}, ${2:ty}, ${3:tz});",
\           "trfo": "transform-origin:|;",
\           "trfs": "transform-style:${1:preserve-3d};",
\           "trs": "transition:${1:prop} ${2:time};",
\           "trsde": "transition-delay:${1:time};",
\           "trsdu": "transition-duration:${1:time};",
\           "trsp": "transition-property:${1:prop};",
\           "trstf": "transition-timing-function:${1:tfunc};",
\           "lh": "line-height:|;",
\           "whs": "white-space:|;",
\           "whs:n": "white-space:normal;",
\           "whs:p": "white-space:pre;",
\           "whs:nw": "white-space:nowrap;",
\           "whs:pw": "white-space:pre-wrap;",
\           "whs:pl": "white-space:pre-line;",
\           "whsc": "white-space-collapse:|;",
\           "whsc:n": "white-space-collapse:normal;",
\           "whsc:k": "white-space-collapse:keep-all;",
\           "whsc:l": "white-space-collapse:loose;",
\           "whsc:bs": "white-space-collapse:break-strict;",
\           "whsc:ba": "white-space-collapse:break-all;",
\           "wob": "word-break:|;",
\           "wob:n": "word-break:normal;",
\           "wob:k": "word-break:keep-all;",
\           "wob:ba": "word-break:break-all;",
\           "wos": "word-spacing:|;",
\           "wow": "word-wrap:|;",
\           "wow:nm": "word-wrap:normal;",
\           "wow:n": "word-wrap:none;",
\           "wow:u": "word-wrap:unrestricted;",
\           "wow:s": "word-wrap:suppress;",
\           "wow:b": "word-wrap:break-word;",
\           "wm": "writing-mode:${1:lr-tb};",
\           "wm:lrt": "writing-mode:lr-tb;",
\           "wm:lrb": "writing-mode:lr-bt;",
\           "wm:rlt": "writing-mode:rl-tb;",
\           "wm:rlb": "writing-mode:rl-bt;",
\           "wm:tbr": "writing-mode:tb-rl;",
\           "wm:tbl": "writing-mode:tb-lr;",
\           "wm:btl": "writing-mode:bt-lr;",
\           "wm:btr": "writing-mode:bt-rl;",
\           "lts": "letter-spacing:|;",
\           "lts-n": "letter-spacing:normal;",
\           "f": "font:|;",
\           "f+": "font:${1:1em} ${2:Arial,sans-serif};",
\           "fw": "font-weight:|;",
\           "fw:n": "font-weight:normal;",
\           "fw:b": "font-weight:bold;",
\           "fw:br": "font-weight:bolder;",
\           "fw:lr": "font-weight:lighter;",
\           "fs": "font-style:${italic};",
\           "fs:n": "font-style:normal;",
\           "fs:i": "font-style:italic;",
\           "fs:o": "font-style:oblique;",
\           "fv": "font-variant:|;",
\           "fv:n": "font-variant:normal;",
\           "fv:sc": "font-variant:small-caps;",
\           "fz": "font-size:|;",
\           "fza": "font-size-adjust:|;",
\           "fza:n": "font-size-adjust:none;",
\           "ff": "font-family:|;",
\           "ff:s": "font-family:serif;",
\           "ff:ss": "font-family:sans-serif;",
\           "ff:c": "font-family:cursive;",
\           "ff:f": "font-family:fantasy;",
\           "ff:m": "font-family:monospace;",
\           "ff:a": "font-family: Arial, \"Helvetica Neue\", Helvetica, sans-serif;",
\           "ff:t": "font-family: \"Times New Roman\", Times, Baskerville, Georgia, serif;",
\           "ff:v": "font-family: Verdana, Geneva, sans-serif;",
\           "fef": "font-effect:|;",
\           "fef:n": "font-effect:none;",
\           "fef:eg": "font-effect:engrave;",
\           "fef:eb": "font-effect:emboss;",
\           "fef:o": "font-effect:outline;",
\           "fem": "font-emphasize:|;",
\           "femp": "font-emphasize-position:|;",
\           "femp:b": "font-emphasize-position:before;",
\           "femp:a": "font-emphasize-position:after;",
\           "fems": "font-emphasize-style:|;",
\           "fems:n": "font-emphasize-style:none;",
\           "fems:ac": "font-emphasize-style:accent;",
\           "fems:dt": "font-emphasize-style:dot;",
\           "fems:c": "font-emphasize-style:circle;",
\           "fems:ds": "font-emphasize-style:disc;",
\           "fsm": "font-smooth:|;",
\           "fsm:a": "font-smooth:auto;",
\           "fsm:n": "font-smooth:never;",
\           "fsm:aw": "font-smooth:always;",
\           "fst": "font-stretch:|;",
\           "fst:n": "font-stretch:normal;",
\           "fst:uc": "font-stretch:ultra-condensed;",
\           "fst:ec": "font-stretch:extra-condensed;",
\           "fst:c": "font-stretch:condensed;",
\           "fst:sc": "font-stretch:semi-condensed;",
\           "fst:se": "font-stretch:semi-expanded;",
\           "fst:e": "font-stretch:expanded;",
\           "fst:ee": "font-stretch:extra-expanded;",
\           "fst:ue": "font-stretch:ultra-expanded;",
\           "op": "opacity:|;",
\           "op+": "opacity: $1;\nfilter: alpha(opacity=$2);",
\           "op:ie": "filter:progid:DXImageTransform.Microsoft.Alpha(Opacity=100);",
\           "op:ms": "-ms-filter:'progid:DXImageTransform.Microsoft.Alpha(Opacity=100)';",
\           "rsz": "resize:|;",
\           "rsz:n": "resize:none;",
\           "rsz:b": "resize:both;",
\           "rsz:h": "resize:horizontal;",
\           "rsz:v": "resize:vertical;",
\           "cur": "cursor:${pointer};",
\           "cur:a": "cursor:auto;",
\           "cur:d": "cursor:default;",
\           "cur:c": "cursor:crosshair;",
\           "cur:ha": "cursor:hand;",
\           "cur:he": "cursor:help;",
\           "cur:m": "cursor:move;",
\           "cur:p": "cursor:pointer;",
\           "cur:t": "cursor:text;",
\           "pgbb": "page-break-before:|;",
\           "pgbb:au": "page-break-before:auto;",
\           "pgbb:al": "page-break-before:always;",
\           "pgbb:l": "page-break-before:left;",
\           "pgbb:r": "page-break-before:right;",
\           "pgbi": "page-break-inside:|;",
\           "pgbi:au": "page-break-inside:auto;",
\           "pgbi:av": "page-break-inside:avoid;",
\           "pgba": "page-break-after:|;",
\           "pgba:au": "page-break-after:auto;",
\           "pgba:al": "page-break-after:always;",
\           "pgba:l": "page-break-after:left;",
\           "pgba:r": "page-break-after:right;",
\           "orp": "orphans:|;",
\           "us": "user-select:${none};",
\           "wid": "widows:|;",
\           "wfsm": "-webkit-font-smoothing:${antialiased};",
\           "wfsm:a": "-webkit-font-smoothing:antialiased;",
\           "wfsm:s": "-webkit-font-smoothing:subpixel-antialiased;",
\           "wfsm:sa": "-webkit-font-smoothing:subpixel-antialiased;",
\           "wfsm:n": "-webkit-font-smoothing:none;"
\        },
\        'filters': 'fc',
\    },
\    'sass': {
\        'extends': 'css',
\        'snippets': {
\            '@if': "@if {\n\t|\n}",
\            '@e': "@else {\n\t|\n}",
\            '@in': "@include |",
\            '@ex': "@extend |",
\            '@mx': "@mixin {\n\t|\n}",
\            '@fn': "@function {\n\t|\n}",
\            '@r': "@return |",
\        },
\    },
\    'scss': {
\        'extends': 'css',
\    },
\    'less': {
\        'extends': 'css',
\    },
\    'css.drupal': {
\        'extends': 'css',
\    },
\    'html': {
\        'snippets': {
\            '!': "html:5",
\            '!!!': "<!DOCTYPE html>\n",
\            '!!!4t':  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n",
\            '!!!4s':  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n",
\            '!!!xt':  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n",
\            '!!!xs':  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n",
\            '!!!xxs': "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n",
\            'c': "<!-- |${child} -->",
\            'cc:ie6': "<!--[if lte IE 6]>\n\t${child}|\n<![endif]-->",
\            'cc:ie': "<!--[if IE]>\n\t${child}|\n<![endif]-->",
\            'cc:noie': "<!--[if !IE]><!-->\n\t${child}|\n<!--<![endif]-->",
\            'html:4t': "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n"
\                    ."<html lang=\"${lang}\">\n"
\                    ."<head>\n"
\                    ."\t<meta http-equiv=\"Content-Type\" content=\"text/html;charset=${charset}\">\n"
\                    ."\t<title></title>\n"
\                    ."</head>\n"
\                    ."<body>\n\t${child}|\n</body>\n"
\                    ."</html>",
\            'html:4s': "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n"
\                    ."<html lang=\"${lang}\">\n"
\                    ."<head>\n"
\                    ."\t<meta http-equiv=\"Content-Type\" content=\"text/html;charset=${charset}\">\n"
\                    ."\t<title></title>\n"
\                    ."</head>\n"
\                    ."<body>\n\t${child}|\n</body>\n"
\                    ."</html>",
\            'html:xt': "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
\                    ."<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${lang}\">\n"
\                    ."<head>\n"
\                    ."\t<meta http-equiv=\"Content-Type\" content=\"text/html;charset=${charset}\" />\n"
\                    ."\t<title></title>\n"
\                    ."</head>\n"
\                    ."<body>\n\t${child}|\n</body>\n"
\                    ."</html>",
\            'html:xs': "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
\                    ."<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${lang}\">\n"
\                    ."<head>\n"
\                    ."\t<meta http-equiv=\"Content-Type\" content=\"text/html;charset=${charset}\" />\n"
\                    ."\t<title></title>\n"
\                    ."</head>\n"
\                    ."<body>\n\t${child}|\n</body>\n"
\                    ."</html>",
\            'html:xxs': "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n"
\                    ."<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${lang}\">\n"
\                    ."<head>\n"
\                    ."\t<meta http-equiv=\"Content-Type\" content=\"text/html;charset=${charset}\" />\n"
\                    ."\t<title></title>\n"
\                    ."</head>\n"
\                    ."<body>\n\t${child}|\n</body>\n"
\                    ."</html>",
\            'html:5': "<!DOCTYPE html>\n"
\                    ."<html lang=\"${lang}\">\n"
\                    ."<head>\n"
\                    ."\t<meta charset=\"${charset}\">\n"
\                    ."\t<title></title>\n"
\                    ."</head>\n"
\                    ."<body>\n\t${child}|\n</body>\n"
\                    ."</html>",
\        },
\        'default_attributes': {
\            'a': [{'href': ''}],
\            'a:link': [{'href': 'http://|'}],
\            'a:mail': [{'href': 'mailto:|'}],
\            'abbr': [{'title': ''}],
\            'acronym': [{'title': ''}],
\            'base': [{'href': ''}],
\            'bdo': [{'dir': ''}],
\            'bdo:r': [{'dir': 'rtl'}],
\            'bdo:l': [{'dir': 'ltr'}],
\            'del': [{'datetime': '${datetime}'}],
\            'ins': [{'datetime': '${datetime}'}],
\            'link:css': [{'rel': 'stylesheet'}, g:emmet_html5 ? {} : {'type': 'text/css'}, {'href': '|style.css'}, {'media': 'all'}],
\            'link:print': [{'rel': 'stylesheet'}, g:emmet_html5 ? {} : {'type': 'text/css'}, {'href': '|print.css'}, {'media': 'print'}],
\            'link:import': [{'rel': 'import'}, {'href': '|.html'}],
\            'link:im': [{'rel': 'import'}, {'href': '|.html'}],
\            'link:favicon': [{'rel': 'shortcut icon'}, {'type': 'image/x-icon'}, {'href': '|favicon.ico'}],
\            'link:touch': [{'rel': 'apple-touch-icon'}, {'href': '|favicon.png'}],
\            'link:rss': [{'rel': 'alternate'}, {'type': 'application/rss+xml'}, {'title': 'RSS'}, {'href': '|rss.xml'}],
\            'link:atom': [{'rel': 'alternate'}, {'type': 'application/atom+xml'}, {'title': 'Atom'}, {'href': 'atom.xml'}],
\            'meta:utf': [{'http-equiv': 'Content-Type'}, {'content': 'text/html;charset=UTF-8'}],
\            'meta:vp': [{'name': 'viewport'}, {'content': 'width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'}],
\            'meta:win': [{'http-equiv': 'Content-Type'}, {'content': 'text/html;charset=Win-1251'}],
\            'meta:compat': [{'http-equiv': 'X-UA-Compatible'}, {'content': 'IE=7'}],
\            'style': g:emmet_html5 ? [] : [{'type': 'text/css'}],
\            'script': [{'src': ''}] + (g:emmet_html5 ? [] : [{'type': 'text/javascript'}]),
\            'script:src': [{'src': ''}] + (g:emmet_html5 ? [] : [{'type': 'text/javascript'}, {'src': ''}]),
\            'img': [{'src': ''}, {'alt': ''}],
\            'iframe': [{'src': ''}, {'frameborder': '0'}],
\            'embed': [{'src': ''}, {'type': ''}],
\            'object': [{'data': ''}, {'type': ''}],
\            'param': [{'name': ''}, {'value': ''}],
\            'map': {'name': ''},
\            'area': [{'shape': ''}, {'coords': ''}, {'href': ''}, {'alt': ''}],
\            'area:d': [{'shape': 'default'}, {'href': ''}, {'alt': ''}],
\            'area:c': [{'shape': 'circle'}, {'coords': ''}, {'href': ''}, {'alt': ''}],
\            'area:r': [{'shape': 'rect'}, {'coords': ''}, {'href': ''}, {'alt': ''}],
\            'area:p': [{'shape': 'poly'}, {'coords': ''}, {'href': ''}, {'alt': ''}],
\            'link': [{'rel': 'stylesheet'}, {'href': ''}],
\            'form': [{'action': ''}],
\            'form:get': [{'action': ''}, {'method': 'get'}],
\            'form:post': [{'action': ''}, {'method': 'post'}],
\            'form:upload': [{'action': ''}, {'method': 'post'}, {'enctype': 'multipart/form-data'}],
\            'label': [{'for': ''}],
\            'input': [{'type': ''}],
\            'input:hidden': [{'type': 'hidden'}, {'name': ''}],
\            'input:h': [{'type': 'hidden'}, {'name': ''}],
\            'input:text': [{'type': 'text'}, {'name': ''}, {'id': ''}],
\            'input:t': [{'type': 'text'}, {'name': ''}, {'id': ''}],
\            'input:search': [{'type': 'search'}, {'name': ''}, {'id': ''}],
\            'input:email': [{'type': 'email'}, {'name': ''}, {'id': ''}],
\            'input:url': [{'type': 'url'}, {'name': ''}, {'id': ''}],
\            'input:password': [{'type': 'password'}, {'name': ''}, {'id': ''}],
\            'input:p': [{'type': 'password'}, {'name': ''}, {'id': ''}],
\            'input:datetime': [{'type': 'datetime'}, {'name': ''}, {'id': ''}],
\            'input:date': [{'type': 'date'}, {'name': ''}, {'id': ''}],
\            'input:datetime-local': [{'type': 'datetime-local'}, {'name': ''}, {'id': ''}],
\            'input:month': [{'type': 'month'}, {'name': ''}, {'id': ''}],
\            'input:week': [{'type': 'week'}, {'name': ''}, {'id': ''}],
\            'input:time': [{'type': 'time'}, {'name': ''}, {'id': ''}],
\            'input:number': [{'type': 'number'}, {'name': ''}, {'id': ''}],
\            'input:color': [{'type': 'color'}, {'name': ''}, {'id': ''}],
\            'input:checkbox': [{'type': 'checkbox'}, {'name': ''}, {'id': ''}],
\            'input:c': [{'type': 'checkbox'}, {'name': ''}, {'id': ''}],
\            'input:radio': [{'type': 'radio'}, {'name': ''}, {'id': ''}],
\            'input:r': [{'type': 'radio'}, {'name': ''}, {'id': ''}],
\            'input:range': [{'type': 'range'}, {'name': ''}, {'id': ''}],
\            'input:file': [{'type': 'file'}, {'name': ''}, {'id': ''}],
\            'input:f': [{'type': 'file'}, {'name': ''}, {'id': ''}],
\            'input:submit': [{'type': 'submit'}, {'value': ''}],
\            'input:s': [{'type': 'submit'}, {'value': ''}],
\            'input:image': [{'type': 'image'}, {'src': ''}, {'alt': ''}],
\            'input:i': [{'type': 'image'}, {'src': ''}, {'alt': ''}],
\            'input:reset': [{'type': 'reset'}, {'value': ''}],
\            'input:button': [{'type': 'button'}, {'value': ''}],
\            'input:b': [{'type': 'button'}, {'value': ''}],
\            'select': [{'name': ''}, {'id': ''}],
\            'option': [{'value': ''}],
\            'textarea': [{'name': ''}, {'id': ''}, {'cols': '30'}, {'rows': '10'}],
\            'menu:context': [{'type': 'context'}],
\            'menu:c': [{'type': 'context'}],
\            'menu:toolbar': [{'type': 'toolbar'}],
\            'menu:t': [{'type': 'toolbar'}],
\            'video': [{'src': ''}],
\            'audio': [{'src': ''}],
\            'html:xml': [{'xmlns': 'http://www.w3.org/1999/xhtml'}, {'xml:lang': '${lang}'}],
\        },
\        'aliases': {
\            'link:*': 'link',
\            'meta:*': 'meta',
\            'area:*': 'area',
\            'bdo:*': 'bdo',
\            'form:*': 'form',
\            'input:*': 'input',
\            'script:*': 'script',
\            'html:*': 'html',
\            'a:*': 'a',
\            'menu:*': 'menu',
\            'bq': 'blockquote',
\            'acr': 'acronym',
\            'fig': 'figure',
\            'ifr': 'iframe',
\            'emb': 'embed',
\            'obj': 'object',
\            'src': 'source',
\            'cap': 'caption',
\            'colg': 'colgroup',
\            'fst': 'fieldset',
\            'btn': 'button',
\            'optg': 'optgroup',
\            'opt': 'option',
\            'tarea': 'textarea',
\            'leg': 'legend',
\            'sect': 'section',
\            'art': 'article',
\            'hdr': 'header',
\            'ftr': 'footer',
\            'adr': 'address',
\            'dlg': 'dialog',
\            'str': 'strong',
\            'sty': 'style',
\            'prog': 'progress',
\            'fset': 'fieldset',
\            'datag': 'datagrid',
\            'datal': 'datalist',
\            'kg': 'keygen',
\            'out': 'output',
\            'det': 'details',
\            'cmd': 'command',
\        },
\        'expandos': {
\            'ol': 'ol>li',
\            'ul': 'ul>li',
\            'dl': 'dl>dt+dd',
\            'map': 'map>area',
\            'table': 'table>tr>td',
\            'colgroup': 'colgroup>col',
\            'colg': 'colgroup>col',
\            'tr': 'tr>td',
\            'select': 'select>option',
\            'optgroup': 'optgroup>option',
\            'optg': 'optgroup>option',
\        },
\        'empty_elements': 'area,base,basefont,br,col,frame,hr,img,input,isindex,link,meta,param,embed,keygen,command',
\        'block_elements': 'address,applet,blockquote,button,center,dd,del,dir,div,dl,dt,fieldset,form,frameset,hr,iframe,ins,isindex,li,link,map,menu,noframes,noscript,object,ol,p,pre,script,table,tbody,td,tfoot,th,thead,tr,ul,h1,h2,h3,h4,h5,h6',
\        'inline_elements': 'a,abbr,acronym,applet,b,basefont,bdo,big,br,button,cite,code,del,dfn,em,font,i,iframe,img,input,ins,kbd,label,map,object,q,s,samp,script,small,span,strike,strong,sub,sup,textarea,tt,u,var',
\        'empty_element_suffix': g:emmet_html5 ? '>' : ' />',
\        'indent_blockelement': 0,
\    },
\    'htmldjango': {
\        'extends': 'html',
\    },
\    'html.django_template': {
\        'extends': 'html',
\    },
\    'jade': {
\        'indentation': '  ',
\        'extends': 'html',
\        'snippets': {
\            '!!!': "doctype html\n",
\            '!!!4t': "doctype HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\"\n",
\            '!!!4s': "doctype HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\"\n",
\            '!!!xt': "doctype transitional\n",
\            '!!!xs': "doctype strict\n",
\            '!!!xxs': "doctype 1.1\n",
\            'c': "\/\/ |${child}",
\            'html:4t': "doctype HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\"\n"
\                    ."html(lang=\"${lang}\")\n"
\                    ."\thead\n"
\                    ."\t\tmeta(http-equiv=\"Content-Type\", content=\"text/html;charset=${charset}\")\n"
\                    ."\t\ttitle\n"
\                    ."\tbody\n\t\t${child}|",
\            'html:4s': "doctype HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\"\n"
\                    ."html(lang=\"${lang}\")\n"
\                    ."\thead\n"
\                    ."\t\tmeta(http-equiv=\"Content-Type\", content=\"text/html;charset=${charset}\")\n"
\                    ."\t\ttitle\n"
\                    ."\tbody\n\t\t${child}|",
\            'html:xt': "doctype transitional\n"
\                    ."html(xmlns=\"http://www.w3.org/1999/xhtml\", xml:lang=\"${lang}\")\n"
\                    ."\thead\n"
\                    ."\t\tmeta(http-equiv=\"Content-Type\", content=\"text/html;charset=${charset}\")\n"
\                    ."\t\ttitle\n"
\                    ."\tbody\n\t\t${child}|",
\            'html:xs': "doctype strict\n"
\                    ."html(xmlns=\"http://www.w3.org/1999/xhtml\", xml:lang=\"${lang}\")\n"
\                    ."\thead\n"
\                    ."\t\tmeta(http-equiv=\"Content-Type\", content=\"text/html;charset=${charset}\")\n"
\                    ."\t\ttitle\n"
\                    ."\tbody\n\t\t${child}|",
\            'html:xxs': "doctype 1.1\n"
\                    ."html(xmlns=\"http://www.w3.org/1999/xhtml\", xml:lang=\"${lang}\")\n"
\                    ."\thead\n"
\                    ."\t\tmeta(http-equiv=\"Content-Type\", content=\"text/html;charset=${charset}\")\n"
\                    ."\t\ttitle\n"
\                    ."\tbody\n\t\t${child}|",
\            'html:5': "doctype html\n"
\                    ."html(lang=\"${lang}\")\n"
\                    ."\thead\n"
\                    ."\t\tmeta(charset=\"${charset}\")\n"
\                    ."\t\ttitle\n"
\                    ."\tbody\n\t\t${child}|",
\        },
\    },
\    'pug': {
\        'extends': 'jade',
\    },
\    'xsl': {
\        'extends': 'html',
\        'default_attributes': {
\            'tmatch': [{'match': ''}, {'mode': ''}],
\            'tname': [{'name': ''}],
\            'xsl:when': {'test': ''},
\            'var': [{'name': ''}, {'select': ''}],
\            'vari': {'name': ''},
\            'if': {'test': ''},
\            'call': {'name': ''},
\            'attr': {'name': ''},
\            'wp': [{'name': ''}, {'select': ''}],
\            'par': [{'name': ''}, {'select': ''}],
\            'val': {'select': ''},
\            'co': {'select': ''},
\            'each': {'select': ''},
\            'ap': [{'select': ''}, {'mode': ''}]
\        },
\        'aliases': {
\            'tmatch': 'xsl:template',
\            'tname': 'xsl:template',
\            'var': 'xsl:variable',
\            'vari': 'xsl:variable',
\            'if': 'xsl:if',
\            'choose': 'xsl:choose',
\            'call': 'xsl:call-template',
\            'wp': 'xsl:with-param',
\            'par': 'xsl:param',
\            'val': 'xsl:value-of',
\            'attr': 'xsl:attribute',
\            'co' : 'xsl:copy-of',
\            'each' : 'xsl:for-each',
\            'ap' : 'xsl:apply-templates',
\        },
\        'expandos': {
\            'choose': 'xsl:choose>xsl:when+xsl:otherwise',
\        }
\    },
\    'jsx': {
\        'extends': 'html',
\        'attribute_name': {'class': 'className', 'for': 'htmlFor'},
\    },
\    'xslt': {
\        'extends': 'xsl',
\    },
\    'haml': {
\        'indentation': '  ',
\        'extends': 'html',
\        'snippets': {
\            'html:5': "!!! 5\n"
\                    ."%html{:lang => \"${lang}\"}\n"
\                    ."\t%head\n"
\                    ."\t\t%meta{:charset => \"${charset}\"}\n"
\                    ."\t\t%title\n"
\                    ."\t%body\n"
\                    ."\t\t${child}|\n",
\        },
\        'attribute_style': 'hash',
\    },
\    'slim': {
\        'indentation': '  ',
\        'extends': 'html',
\        'snippets': {
\            'html:5': "doctype 5\n"
\                    ."html lang=\"${lang}\"\n"
\                    ."\thead\n"
\                    ."\t\tmeta charset=\"${charset}\"\n"
\                    ."\t\ttitle\n"
\                    ."\tbody\n"
\                    ."\t\t${child}|\n",
\        },
\    },
\    'xhtml': {
\        'extends': 'html'
\    },
\    'mustache': {
\        'extends': 'html'
\    },
\    'xsd': {
\        'extends': 'html',
\        'snippets': {
\            'xsd:w3c': "<?xml version=\"1.0\"?>\n"
\                    ."<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\">\n"
\                    ."\t<xsd:element name=\"\" type=\"\"/>\n"
\                    ."</xsd:schema>\n"
\        }
\    },
\}

if exists('g:user_emmet_settings')
  call emmet#mergeConfig(s:emmet_settings, g:user_emmet_settings)
endif

let &cpoptions = s:save_cpo
unlet s:save_cpo

" vim:set et:
