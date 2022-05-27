" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#uri#Encode(value) abort
    return s:encode(a:value, '[^A-Za-z0-9_.~-]')
endfunction

function! go#uri#EncodePath(value) abort
    let l:separator = '/'
    if go#util#IsWin()
      let l:separator = '\\'
    endif
    return s:encode(a:value, '[^' . l:separator . 'A-Za-z0-9_.~-]')
endfunction

function! s:encode(value, unreserved)
    return substitute(
    \   a:value,
    \   a:unreserved,
    \   '\=s:encodechar(submatch(0))',
    \   'g'
    \)
endfunction

function! go#uri#Decode(value) abort
    return substitute(
    \   a:value,
    \   '%\(\x\x\)',
    \   '\=s:decodehex(submatch(1))',
    \   'g'
    \)
endfunction

function! s:encodechar(value)
  let l:idx = 0
  let l:encoded = ''
  while l:idx < strlen(a:value)
    let l:byte = strpart(a:value, l:idx, 1)
    let l:encoded = printf('%s%%%02X', l:encoded, char2nr(l:byte))
    let l:idx += 1
  endwhile

  return l:encoded
endfunction

function! s:decodehex(value)
  return eval(printf('"\x%s"', a:value))
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
