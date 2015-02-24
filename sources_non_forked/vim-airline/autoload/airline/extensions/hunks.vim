" MIT License. Copyright (c) 2013-2015 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !get(g:, 'loaded_signify', 0) && !get(g:, 'loaded_gitgutter', 0) && !get(g:, 'loaded_changes', 0)
  finish
endif

let s:non_zero_only = get(g:, 'airline#extensions#hunks#non_zero_only', 0)
let s:hunk_symbols = get(g:, 'airline#extensions#hunks#hunk_symbols', ['+', '~', '-'])

function! s:get_hunks_signify()
  let hunks = sy#repo#get_stats()
  if hunks[0] >= 0
    return hunks
  endif
  return []
endfunction

function! s:is_branch_empty()
  return exists('*airline#extensions#branch#head') && empty(airline#extensions#branch#head())
endfunction

function! s:get_hunks_gitgutter()
  if !get(g:, 'gitgutter_enabled', 0) || s:is_branch_empty()
    return ''
  endif
  return GitGutterGetHunkSummary()
endfunction

function! s:get_hunks_changes()
  if !get(b:, 'changes_view_enabled', 0) || s:is_branch_empty()
    return []
  endif
  let hunks = changes#GetStats()
  for i in hunks
    if i > 0
      return hunks
    endif
  endfor
  return []
endfunction

function! s:get_hunks_empty()
  return ''
endfunction

let s:source_func = ''
function! s:get_hunks()
  if empty(s:source_func)
    if get(g:, 'loaded_signify', 0)
      let s:source_func = 's:get_hunks_signify'
    elseif exists('*GitGutterGetHunkSummary')
      let s:source_func = 's:get_hunks_gitgutter'
    elseif exists('*changes#GetStats')
      let s:source_func = 's:get_hunks_changes'
    else
      let s:source_func = 's:get_hunks_empty'
    endif
  endif
  return {s:source_func}()
endfunction

function! airline#extensions#hunks#get_hunks()
  if !get(w:, 'airline_active', 0)
    return ''
  endif
  let hunks = s:get_hunks()
  let string = ''
  if !empty(hunks)
    for i in [0, 1, 2]
      if s:non_zero_only == 0 || hunks[i] > 0
        let string .= printf('%s%s ', s:hunk_symbols[i], hunks[i])
      endif
    endfor
  endif
  return string
endfunction

function! airline#extensions#hunks#init(ext)
  call airline#parts#define_function('hunks', 'airline#extensions#hunks#get_hunks')
endfunction

