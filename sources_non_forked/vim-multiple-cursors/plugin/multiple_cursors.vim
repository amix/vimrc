"===============================================================================
" File: multiple_cursors.vim
" Author: Terry Ma
" Description: Emulate Sublime Text's multi selection feature 
" Potential Features:
" - Create a blinking cursor effect? Good place to do it would be instead of
"   waiting for user input, cycle through the highlight
" - Integrate with the status line? Maybe show a special multicursor mode?
" - Support mouse? Ctrl/Cmd click to set cursor?
"===============================================================================
let s:save_cpo = &cpo
set cpo&vim

function! s:init_settings(settings)
  for [key, value] in items(a:settings)
    let sub = ''
    if type(value) == 0
      let sub = '%d'
    elseif type(value) == 1
      let sub = '"%s"'
    endif
    let fmt = printf("let g:multi_cursor_%%s=get(g:, 'multi_cursor_%%s', %s)",
          \ sub)
    exec printf(fmt, key, key, value)
  endfor
endfunction

" Settings
let s:settings = {
      \ 'exit_from_visual_mode': 1,
      \ 'exit_from_insert_mode': 1,
      \ 'use_default_mapping': 1,
      \ 'debug_latency': 0,
      \ }

let s:settings_if_default = {
      \ 'quit_key': '<Esc>',
      \ 'next_key': '<C-n>',
      \ 'prev_key': '<C-p>',
      \ 'skip_key': '<C-x>',
      \ }

call s:init_settings(s:settings)

if g:multi_cursor_use_default_mapping
  call s:init_settings(s:settings_if_default)
endif

if !exists('g:multi_cursor_start_key') && exists('g:multi_cursor_next_key')
  let g:multi_cursor_start_key = g:multi_cursor_next_key
endif

" External mappings
if exists('g:multi_cursor_start_key')
  exec 'nnoremap <silent> '.g:multi_cursor_start_key.
        \' :call multiple_cursors#new("n")<CR>'
  exec 'xnoremap <silent> '.g:multi_cursor_start_key.
        \' :<C-u>call multiple_cursors#new("v")<CR>'
endif

" Commands
command! -nargs=1 -range=% MultipleCursorsFind 
      \ call multiple_cursors#find(<line1>, <line2>, <q-args>)

let &cpo = s:save_cpo
unlet s:save_cpo
