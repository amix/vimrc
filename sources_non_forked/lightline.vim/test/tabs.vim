let s:suite = themis#suite('tabs')
let s:assert = themis#helper('assert')

function! s:suite.before_each()
  set columns=180
  call lightline#init()
  tabnew
  tabonly
endfunction

function! s:tab(number, ...) abort
  let active = get(a:000, 0, 0)
  let last = get(a:000, 1, 0)
  return '%' . a:number . 'T%{lightline#onetab(' . a:number . ',' . active . ')}' . (last ? '%T' : '')
endfunction

function! s:suite.tabs()
  call s:assert.equals(lightline#tabs(), [[], [s:tab(1, 1, 1)], []])
endfunction

function! s:suite.tabnew()
  tabnew
  call s:assert.equals(lightline#tabs(), [[s:tab(1)], [s:tab(2, 1, 1)], []])
endfunction

function! s:suite.tabnew_tabnew()
  tabnew
  tabnew
  call s:assert.equals(lightline#tabs(), [[s:tab(1), s:tab(2)], [s:tab(3, 1, 1)], []])
endfunction

function! s:suite.tabnew_tabfirst()
  tabnew
  tabfirst
  call s:assert.equals(lightline#tabs(), [[], [s:tab(1, 1)], [s:tab(2, 0, 1)]])
endfunction

function! s:suite.tabnew_tabnew_tabfirst()
  tabnew
  tabnew
  tabfirst
  call s:assert.equals(lightline#tabs(), [[], [s:tab(1, 1)], [s:tab(2), s:tab(3, 0, 1)]])
endfunction

function! s:suite.tabnew_tabnew_tabprevious()
  tabnew
  tabnew
  tabprevious
  call s:assert.equals(lightline#tabs(), [[s:tab(1)], [s:tab(2, 1)], [s:tab(3, 0, 1)]])
endfunction

function! s:suite.tabnew_20()
  for i in range(19)
    tabnew
  endfor
  call s:assert.equals(lightline#tabs(), [[s:tab(1), s:tab(2), s:tab(3), s:tab(4), '...', s:tab(16), s:tab(17), s:tab(18), s:tab(19)], [s:tab(20, 1, 1)], []])
endfunction

function! s:suite.tabnew_20_tabfirst()
  for i in range(19)
    tabnew
  endfor
  tabfirst
  call s:assert.equals(lightline#tabs(), [[], [s:tab(1, 1)], [s:tab(2), s:tab(3), s:tab(4), '%<' . s:tab(5), '...', '%<' . s:tab(17), '%<' . s:tab(18), '%<' . s:tab(19), '%<' . s:tab(20, 0, 1)]])
endfunction

function! s:suite.tabnew_20_tabfirst_tabnext()
  for i in range(19)
    tabnew
  endfor
  tabfirst
  tabnext
  call s:assert.equals(lightline#tabs(), [[s:tab(1)], [s:tab(2, 1)], [s:tab(3), s:tab(4), s:tab(5), '%<' . s:tab(6), '...', '%<' . s:tab(18), '%<' . s:tab(19), '%<' . s:tab(20, 0, 1)]])
endfunction

function! s:suite.tabnew_20_tabnext_10()
  for i in range(19)
    tabnew
  endfor
  tabnext 10
  call s:assert.equals(lightline#tabs(), [[s:tab(1), s:tab(2), '...', s:tab(8), s:tab(9)], [s:tab(10, 1)], [s:tab(11), s:tab(12), '...', '%<' . s:tab(19), '%<' . s:tab(20, 0, 1)]])
endfunction

function! s:suite.tabnew_20_tabprevious()
  for i in range(19)
    tabnew
  endfor
  tabprevious
  call s:assert.equals(lightline#tabs(), [[s:tab(1), s:tab(2), s:tab(3), '...', s:tab(15), s:tab(16), s:tab(17), s:tab(18)], [s:tab(19, 1)], [s:tab(20, 0, 1)]])
endfunction

function! s:suite.tabnew_20_tabprevious_tabprevious()
  for i in range(19)
    tabnew
  endfor
  tabprevious
  tabprevious
  call s:assert.equals(lightline#tabs(), [[s:tab(1), s:tab(2), s:tab(3), '...', s:tab(15), s:tab(16), s:tab(17)], [s:tab(18, 1)], [s:tab(19), s:tab(20, 0, 1)]])
endfunction
