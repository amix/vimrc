" json
" Last Change: 2012-03-08
" Maintainer:   Yasuhiro Matsumoto <mattn.jp@gmail.com>
" License:      This file is placed in the public domain.
" Reference:
"
let s:save_cpo = &cpo
set cpo&vim

function! webapi#json#null()
  return 0
endfunction

function! webapi#json#true()
  return 1
endfunction

function! webapi#json#false()
  return 0
endfunction

function! s:nr2byte(nr)
  if a:nr < 0x80
    return nr2char(a:nr)
  elseif a:nr < 0x800
    return nr2char(a:nr/64+192).nr2char(a:nr%64+128)
  else
    return nr2char(a:nr/4096%16+224).nr2char(a:nr/64%64+128).nr2char(a:nr%64+128)
  endif
endfunction

function! s:nr2enc_char(charcode)
  if &encoding == 'utf-8'
    return nr2char(a:charcode)
  endif
  let char = s:nr2byte(a:charcode)
  if strlen(char) > 1
    let char = strtrans(iconv(char, 'utf-8', &encoding))
  endif
  return char
endfunction

function! s:fixup(val, tmp)
  if type(a:val) == 0
    return a:val
  elseif type(a:val) == 1
    if a:val == a:tmp.'null'
      return function('webapi#json#null')
    elseif a:val == a:tmp.'true'
      return function('webapi#json#true')
    elseif a:val == a:tmp.'false'
      return function('webapi#json#false')
    endif
    return a:val
  elseif type(a:val) == 2
    return a:val
  elseif type(a:val) == 3
    return map(a:val, 's:fixup(v:val, a:tmp)')
  elseif type(a:val) == 4
    return map(a:val, 's:fixup(v:val, a:tmp)')
  else
    return string(a:val)
  endif
endfunction

function! webapi#json#decode(json)
  let json = iconv(a:json, "utf-8", &encoding)
  if get(g:, 'webapi#json#parse_strict', 1) == 1 && substitute(substitute(substitute(
    \ json,
    \ '\\\%(["\\/bfnrt]\|u[0-9a-fA-F]\{4}\)', '\@', 'g'),
    \ '"[^\"\\\n\r]*\"\|true\|false\|null\|-\?\d\+'
    \ . '\%(\.\d*\)\?\%([eE][+\-]\{-}\d\+\)\?', ']', 'g'),
    \ '\%(^\|:\|,\)\%(\s*\[\)\+', '', 'g') !~ '^[\],:{} \t\n]*$'
    throw json
  endif
  let json = substitute(json, '\n', '', 'g')
  let json = substitute(json, '\\u34;', '\\"', 'g')
  if v:version >= 703 && has('patch780')
    let json = substitute(json, '\\u\(\x\x\x\x\)', '\=iconv(nr2char(str2nr(submatch(1), 16), 1), "utf-8", &encoding)', 'g')
  else
    let json = substitute(json, '\\u\(\x\x\x\x\)', '\=s:nr2enc_char("0x".submatch(1))', 'g')
  endif
  if get(g:, 'webapi#json#allow_nil', 0) != 0
    let tmp = '__WEBAPI_JSON__'
    while 1
      if stridx(json, tmp) == -1
        break
      endif
      let tmp .= '_'
    endwhile
    let [null,true,false] = [
    \ tmp.'null',
    \ tmp.'true',
    \ tmp.'false']
    sandbox let ret = eval(json)
    call s:fixup(ret, tmp)
  else
    let [null,true,false] = [0,1,0]
    sandbox let ret = eval(json)
  endif
  return ret
endfunction

function! webapi#json#encode(val)
  if type(a:val) == 0
    return a:val
  elseif type(a:val) == 1
    let json = '"' . escape(a:val, '\"') . '"'
    let json = substitute(json, "\r", '\\r', 'g')
    let json = substitute(json, "\n", '\\n', 'g')
    let json = substitute(json, "\t", '\\t', 'g')
    let json = substitute(json, '\([[:cntrl:]]\)', '\=printf("\x%02d", char2nr(submatch(1)))', 'g')
    return iconv(json, &encoding, "utf-8")
  elseif type(a:val) == 2
    let s = string(a:val)
    if s == "function('webapi#json#null')"
      return 'null'
    elseif s == "function('webapi#json#true')"
      return 'true'
    elseif s == "function('webapi#json#false')"
      return 'false'
    endif
  elseif type(a:val) == 3
    return '[' . join(map(copy(a:val), 'webapi#json#encode(v:val)'), ',') . ']'
  elseif type(a:val) == 4
    return '{' . join(map(keys(a:val), 'webapi#json#encode(v:val).":".webapi#json#encode(a:val[v:val])'), ',') . '}'
  else
    return string(a:val)
  endif
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et:
