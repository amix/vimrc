if !exists("g:go_textobj_enabled")
    let g:go_textobj_enabled = 1
endif

function! go#textobj#Function(mode)
  if search('^\s*func .*{$', 'Wce', line('.')) <= 0
        \ && search('^\s*func .*{$', 'bWce') <= 0
    return
  endif
  if a:mode == 'a'
    normal! Va{V
  else " a:mode == 'i'
    normal! Vi{V
  endif
endfunction
