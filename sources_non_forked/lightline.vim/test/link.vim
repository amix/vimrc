let s:suite = themis#suite('link')
let s:assert = themis#helper('assert')

function! s:suite.before_each()
  call s:clear()
  let g:lightline = {}
  call lightline#init()
  call lightline#colorscheme()
endfunction

function! s:clear()
  hi clear
  redir => hi
    silent! hi
  redir END
  for line in split(hi, '\n')
    if match(line, 'links to') > 0
      exec 'hi link' matchstr(line, '^\S*') 'NONE'
    endif
  endfor
endfunction

function! s:hi(name)
  redir => hi
    silent! exec 'hi' a:name
  redir END
  return substitute(join(split(hi, "\n"), ''), ' \+', ' ', 'g')
endfunction

function! s:suite.link()
  call lightline#link()
  call s:assert.match(s:hi('LightlineLeft_active_0'), 'LightlineLeft_normal_0')
  call s:assert.match(s:hi('LightlineLeft_active_1'), 'LightlineLeft_normal_1')
  call s:assert.match(s:hi('LightlineLeft_active_2'), 'E411: highlight group not found\|cleared')
  call s:assert.match(s:hi('LightlineRight_active_0'), 'LightlineRight_normal_0')
  call s:assert.match(s:hi('LightlineRight_active_1'), 'LightlineRight_normal_1')
  call s:assert.match(s:hi('LightlineRight_active_2'), 'LightlineRight_normal_2')
  call s:assert.match(s:hi('LightlineRight_active_3'), 'E411: highlight group not found\|cleared')
  call s:assert.match(s:hi('LightlineMiddle_active'), 'LightlineMiddle_normal')
endfunction

function! s:suite.insert()
  call lightline#link('i')
  call s:assert.match(s:hi('LightlineLeft_active_0'), 'LightlineLeft_insert_0')
  call s:assert.match(s:hi('LightlineLeft_active_1'), 'LightlineLeft_insert_1')
  call s:assert.match(s:hi('LightlineLeft_active_2'), 'E411: highlight group not found\|cleared')
  call s:assert.match(s:hi('LightlineRight_active_0'), 'LightlineRight_insert_0')
  call s:assert.match(s:hi('LightlineRight_active_1'), 'LightlineRight_insert_1')
  call s:assert.match(s:hi('LightlineRight_active_2'), 'LightlineRight_insert_2')
  call s:assert.match(s:hi('LightlineRight_active_3'), 'E411: highlight group not found\|cleared')
  call s:assert.match(s:hi('LightlineMiddle_active'), 'LightlineMiddle_insert')
endfunction

function! s:suite.visual()
  call lightline#link('v')
  call s:assert.match(s:hi('LightlineLeft_active_0'), 'LightlineLeft_visual_0')
  call s:assert.match(s:hi('LightlineLeft_active_1'), 'LightlineLeft_visual_1')
  call s:assert.match(s:hi('LightlineLeft_active_2'), 'E411: highlight group not found\|cleared')
  call s:assert.match(s:hi('LightlineRight_active_0'), 'LightlineRight_visual_0')
  call s:assert.match(s:hi('LightlineRight_active_1'), 'LightlineRight_visual_1')
  call s:assert.match(s:hi('LightlineRight_active_2'), 'LightlineRight_visual_2')
  call s:assert.match(s:hi('LightlineRight_active_3'), 'E411: highlight group not found\|cleared')
  call s:assert.match(s:hi('LightlineMiddle_active'), 'LightlineMiddle_visual')
endfunction

function! s:suite.replace()
  call lightline#link('R')
  call s:assert.match(s:hi('LightlineLeft_active_0'), 'LightlineLeft_replace_0')
  call s:assert.match(s:hi('LightlineLeft_active_1'), 'LightlineLeft_replace_1')
  call s:assert.match(s:hi('LightlineLeft_active_2'), 'E411: highlight group not found\|cleared')
  call s:assert.match(s:hi('LightlineRight_active_0'), 'LightlineRight_replace_0')
  call s:assert.match(s:hi('LightlineRight_active_1'), 'LightlineRight_replace_1')
  call s:assert.match(s:hi('LightlineRight_active_2'), 'LightlineRight_replace_2')
  call s:assert.match(s:hi('LightlineRight_active_3'), 'E411: highlight group not found\|cleared')
  call s:assert.match(s:hi('LightlineMiddle_active'), 'LightlineMiddle_replace')
endfunction

function! s:suite.left_right()
  let g:lightline = {
        \   'active': {
        \     'left': [ [ 'mode', 'paste' ], [ 'readonly' ], [ 'filename' ], [ 'modified' ] ],
        \     'right': [ [ 'lineinfo' ], [ 'percent' ], [ 'fileformat' ], [ 'fileencoding' ], [ 'filetype' ] ]
        \   },
        \ }
  call lightline#init()
  call lightline#colorscheme()
  call lightline#link()
  call s:assert.match(s:hi('LightlineLeft_active_0'), 'LightlineLeft_normal_0')
  call s:assert.match(s:hi('LightlineLeft_active_1'), 'LightlineLeft_normal_1')
  call s:assert.match(s:hi('LightlineLeft_active_2'), 'LightlineLeft_normal_2')
  call s:assert.match(s:hi('LightlineLeft_active_3'), 'LightlineLeft_normal_3')
  call s:assert.match(s:hi('LightlineLeft_active_4'), 'E411: highlight group not found')
  call s:assert.match(s:hi('LightlineRight_active_0'), 'LightlineRight_normal_0')
  call s:assert.match(s:hi('LightlineRight_active_1'), 'LightlineRight_normal_1')
  call s:assert.match(s:hi('LightlineRight_active_2'), 'LightlineRight_normal_2')
  call s:assert.match(s:hi('LightlineRight_active_3'), 'LightlineRight_normal_3')
  call s:assert.match(s:hi('LightlineRight_active_4'), 'LightlineRight_normal_4')
  call s:assert.match(s:hi('LightlineRight_active_5'), 'E411: highlight group not found')
  call s:assert.match(s:hi('LightlineMiddle_active'), 'LightlineMiddle_normal')
endfunction

function! s:suite.subseparator()
  let g:lightline = {
        \   'active': {
        \     'left': [ [ 'mode', 'paste' ], [ 'readonly' ], [ 'filename' ], [ 'modified' ] ],
        \     'right': [ [ 'lineinfo' ], [ 'percent' ], [ 'fileformat' ], [ 'fileencoding' ], [ 'filetype' ] ]
        \   },
        \ }
  call lightline#init()
  call lightline#colorscheme()
  call lightline#link()
  for i in range(4)
    for j in range(5)
      if i + 1 == j
        call s:assert.match(s:hi(printf('LightlineLeft_active_%s_%s', i, j)), printf('LightlineLeft_normal_%s_%s', i, j))
      else
        call s:assert.match(s:hi(printf('LightlineLeft_active_%s_%s', i, j)), 'E411: highlight group not found')
      endif
    endfor
  endfor
endfunction

function! s:suite.component_type()
  let g:lightline = { 'component_type': { 'error': 'error', 'warning': 'warning' } }
  call lightline#init()
  call lightline#colorscheme()
  call lightline#link()
  for type in ['error', 'warning']
    call s:assert.match(s:hi(printf('LightlineLeft_active_%s', type)), printf('LightlineLeft_normal_%s', type))
    call s:assert.match(s:hi(printf('LightlineLeft_active_0_%s', type)), printf('LightlineLeft_normal_0_%s', type))
    call s:assert.match(s:hi(printf('LightlineLeft_active_1_%s', type)), printf('LightlineLeft_normal_1_%s', type))
    call s:assert.match(s:hi(printf('LightlineLeft_active_2_%s', type)), 'E411: highlight group not found')
    call s:assert.match(s:hi(printf('LightlineLeft_active_%s_0', type)), printf('LightlineLeft_normal_%s_0', type))
    call s:assert.match(s:hi(printf('LightlineLeft_active_%s_1', type)), printf('LightlineLeft_normal_%s_1', type))
    call s:assert.match(s:hi(printf('LightlineLeft_active_%s_2', type)), printf('LightlineLeft_normal_%s_2', type))
    call s:assert.match(s:hi(printf('LightlineLeft_active_%s_3', type)), 'E411: highlight group not found')
  endfor
  for type1 in ['error', 'warning']
    for type2 in ['error', 'warning']
      call s:assert.match(s:hi(printf('LightlineLeft_active_%s_%s', type1, type2)), printf('LightlineLeft_normal_%s_%s', type1, type2))
    endfor
  endfor
endfunction

function! s:suite.hi_clear()
  call lightline#link()
  colorscheme default
  call lightline#link()
  call s:assert.match(s:hi('LightlineLeft_active_0'), 'LightlineLeft_normal_0')
  call s:assert.match(s:hi('LightlineLeft_active_1'), 'LightlineLeft_normal_1')
  call s:assert.match(s:hi('LightlineLeft_active_2'), 'E411: highlight group not found\|cleared')
  call s:assert.match(s:hi('LightlineRight_active_0'), 'LightlineRight_normal_0')
  call s:assert.match(s:hi('LightlineRight_active_1'), 'LightlineRight_normal_1')
  call s:assert.match(s:hi('LightlineRight_active_2'), 'LightlineRight_normal_2')
  call s:assert.match(s:hi('LightlineRight_active_3'), 'E411: highlight group not found\|cleared')
  call s:assert.match(s:hi('LightlineMiddle_active'), 'LightlineMiddle_normal')
endfunction
