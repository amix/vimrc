
let s:debug = 0

function! s:CursorHasGroup(group) abort
  return synIDattr(synID(line('.'), col('.'), 0), 'name') =~ a:group
endfunction

function! TestSyntax(pattern, group) abort
  let pattern = '\C' . a:pattern
  call cursor(1, 1)
  redraw
  let start_match = search(pattern, 'c') && s:CursorHasGroup(a:group)
  if s:debug
    redraw | sleep 500m
  endif
  let end_match   = search(pattern, 'e') && s:CursorHasGroup(a:group)
  if s:debug
    redraw | sleep 500m
  endif
  return start_match && end_match
endfunction
