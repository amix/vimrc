let s:suite = themis#suite('onetab')
let s:assert = themis#helper('assert')

function! s:suite.before_each()
  let g:lightline = {}
  call lightline#init()
  tabnew
  tabonly
endfunction

function! s:suite.onetab()
  call s:assert.equals(lightline#onetab(1, 1), '1 [No Name]')
endfunction

function! s:suite.tabnew()
  tabnew
  call s:assert.equals(lightline#onetab(1, 0), '1 [No Name]')
  call s:assert.equals(lightline#onetab(2, 1), '2 [No Name]')
endfunction

function! s:suite.tabnew_tabnew()
  tabnew
  tabnew
  call s:assert.equals(lightline#onetab(1, 0), '1 [No Name]')
  call s:assert.equals(lightline#onetab(2, 0), '2 [No Name]')
  call s:assert.equals(lightline#onetab(3, 1), '3 [No Name]')
endfunction

function! s:suite.modified()
  call append(0, '')
  call s:assert.equals(lightline#onetab(1, 1), '1 [No Name] +')
  undo
endfunction

function! s:suite.filename()
  edit test
  call s:assert.equals(lightline#onetab(1, 1), '1 test')
  tabnew
  bunload test
endfunction

function! s:suite.filename_modified()
  edit test
  call append(0, '')
  call s:assert.equals(lightline#onetab(1, 1), '1 test +')
  tabnew
  bunload! test
endfunction

function! s:suite.active_inactive()
  let g:lightline = { 'tab': { 'active': [ 'tabnum', 'filename' ], 'inactive': [ 'filename' ] } }
  call lightline#init()
  edit test
  call append(0, '')
  call s:assert.equals(lightline#onetab(1, 1), '1 test')
  call s:assert.equals(lightline#onetab(1, 0), 'test')
  tabnew
  bunload! test
endfunction

function! s:suite.tab_component()
  let g:lightline = { 'tab': { 'active': [ 'custom' ] }, 'tab_component': { 'custom': 'custom' } }
  call lightline#init()
  call s:assert.equals(lightline#onetab(1, 1), 'custom')
  call s:assert.equals(lightline#onetab(2, 1), 'custom')
endfunction

function! s:suite.tab_component_function()
  function! Custom(n)
    return 'custom: ' . a:n
  endfunction
  let g:lightline = { 'tab': { 'active': [ 'custom' ] }, 'tab_component_function': { 'custom': 'Custom' } }
  call lightline#init()
  call s:assert.equals(lightline#onetab(1, 1), 'custom: 1')
  call s:assert.equals(lightline#onetab(2, 1), 'custom: 2')
  delfunction Custom
endfunction

function! s:suite.tab_component_empty_middle()
  let g:lightline = { 'tab': { 'active': [ 'tabnum', 'custom', 'filename' ], 'inactive': [ 'tabnum', 'custom', 'custom', 'filename' ] }, 'tab_component': { 'custom': '' } }
  call lightline#init()
  call s:assert.equals(lightline#onetab(1, 1), '1 [No Name]')
  call s:assert.equals(lightline#onetab(2, 1), '2 [No Name]')
endfunction

function! s:suite.tab_component_empty_left()
  let g:lightline = { 'tab': { 'active': [ 'custom', 'filename' ], 'inactive': [ 'custom', 'custom', 'filename' ] }, 'tab_component': { 'custom': '' } }
  call lightline#init()
  call s:assert.equals(lightline#onetab(1, 1), '[No Name]')
  call s:assert.equals(lightline#onetab(2, 1), '[No Name]')
endfunction

function! s:suite.tab_component_empty_middle()
  let g:lightline = { 'tab': { 'active': [ 'tabnum', 'custom' ], 'inactive': [ 'tabnum', 'custom', 'custom' ] }, 'tab_component': { 'custom': '' } }
  call lightline#init()
  call s:assert.equals(lightline#onetab(1, 1), '1')
  call s:assert.equals(lightline#onetab(2, 1), '2')
endfunction
