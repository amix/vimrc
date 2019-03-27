" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#uri#Encode(value) abort
    return s:encode(a:value, '[^A-Za-z0-9_.~-]')
endfunction

function! go#uri#EncodePath(value) abort
    return s:encode(a:value, '[^/A-Za-z0-9_.~-]')
endfunction

function! s:encode(value, unreserved)
    return substitute(
    \   a:value,
    \   a:unreserved,
    \   '\="%".printf(''%02X'', char2nr(submatch(0)))',
    \   'g'
    \)
endfunction

function! go#uri#Decode(value) abort
    return substitute(
    \   a:value,
    \   '%\(\x\x\)',
    \   '\=nr2char(''0X'' . submatch(1))',
    \   'g'
    \)
endfunction
" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
