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

let s:default_insert_maps = {}
let s:default_normal_maps = {'!':1, '@':1, '=':1, 'q':1, 'r':1, 't':1, 'T':1, 'y':1, '[':1, ']':1, '\':1, 'd':1, 'f':1, 'F':1, 'g':1, '"':1, 'z':1, 'c':1, 'm':1, '<':1, '>':1}
let s:default_visual_maps = {'i':1, 'a':1, 'f':1, 'F':1, 't':1, 'T':1}

let g:multi_cursor_insert_maps =
      \ get(g:, 'multi_cursor_insert_maps', s:default_insert_maps)
let g:multi_cursor_normal_maps =
      \ get(g:, 'multi_cursor_normal_maps', s:default_normal_maps)
let g:multi_cursor_visual_maps =
      \ get(g:, 'multi_cursor_visual_maps', s:default_visual_maps)

call s:init_settings(s:settings)

if g:multi_cursor_use_default_mapping
  call s:init_settings(s:settings_if_default)
endif

if !exists('g:multi_cursor_start_word_key')
  if exists('g:multi_cursor_start_key')
    let g:multi_cursor_start_word_key = g:multi_cursor_start_key
  elseif exists('g:multi_cursor_next_key')
    let g:multi_cursor_start_word_key = g:multi_cursor_next_key
  endif
endif

" External mappings
if exists('g:multi_cursor_start_key')
  exec 'nnoremap <silent> '.g:multi_cursor_start_key.
        \' :call multiple_cursors#new("n", 0)<CR>'
  exec 'xnoremap <silent> '.g:multi_cursor_start_key.
        \' :<C-u>call multiple_cursors#new("v", 0)<CR>'
endif

if exists('g:multi_cursor_start_word_key')
  exec 'nnoremap <silent> '.g:multi_cursor_start_word_key.
        \' :call multiple_cursors#new("n", 1)<CR>'
  " In Visual mode word boundary is not used
  exec 'xnoremap <silent> '.g:multi_cursor_start_word_key.
        \' :<C-u>call multiple_cursors#new("v", 0)<CR>'
endif

" Commands
command! -nargs=1 -range=% MultipleCursorsFind
      \ call multiple_cursors#find(<line1>, <line2>, <q-args>)

let &cpo = s:save_cpo
unlet s:save_cpo
