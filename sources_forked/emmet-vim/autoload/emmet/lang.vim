let s:exists = {}
function! emmet#lang#exists(type) abort
  if len(a:type) == 0
    return 0
  elseif has_key(s:exists, a:type)
    return s:exists[a:type]
  endif
  let s:exists[a:type] = len(globpath(&rtp, 'autoload/emmet/lang/'.a:type.'.vim')) > 0
  return s:exists[a:type]
endfunction

