"===============================================================================
" File: multiple_cursors.vim
" Author: Terry Ma
" Description: Emulate Sublime Text's multi selection feature 
" Issues:
" - Performance in terminal vim degrades significantly with more cursors
" - All user input typed before Vim is able to fan out the last operation to all
"   cursors is lost. This is a implementation decision to keep the input
"   perfectly synced in all locations, at the cost of potentially losing user
"   input.
" - Multi key commands is not supported
" - Single key commands that do not terminate properly cause unexpected
"   behavior. For example, if the cursor is on the first character in the buffer
"   and 'b' is pressed.
" - Undo behavior is unpredictable
" - Select mode is not implemented
" - There is a bug with selection and highlight when wrap is on
"
" Potential Features:
" - Create a blinking cursor effect? Good place to do it would be instead of
"   waiting for user input, cycle through the highlight
" - Integrate with the status line? Maybe show a special multicursor mode?
" - Support mouse? Ctrl/Cmd click to set cursor?
"
" Features:
" - Real time update of cursor locations
" - In normal mode, pressing <C-n> will highlight the current word under cursor,
"   and places a 'multicursor' at the end of the word, and goes to visual mode
" - In visual mode, right after the above operation, pressing <C-n> again will
"   search for the word forward, and places a new cursor at the end of the
"   resulting search, one can continue to do this in Visual mode, this resembles
"   the Cmd-D feature of Sublime
" - In insert mode, insert operations are captures and replayed at all the
"   cursor locations
" - Pressing <Esc> in Normal mode quits multicursor mode and clears all cursors
" - Normal mode single keystroke commands work:
"   - Works: 'w,e,i,p,a,h,j,k,l,x,v,b'
"   - Does not work: ''
" - Replace mode just seems to work
" - Visual mode
"   - Works: 'w,e,b,h,j,k,l,o'
"   - Does not work: 'A, I', because <C-o> does not get it out of normal mode
"   for these commands. It takes two
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
      \ }

let s:settings_if_default = {
      \ 'quit_key': "\<Esc>",
      \ 'next_key': "\<C-n>",
      \ 'prev_key': "\<C-p>",
      \ 'skip_key': "\<C-x>",
      \ }

call s:init_settings(s:settings)

if g:multi_cursor_use_default_mapping
  call s:init_settings(s:settings_if_default)
endif

" External mappings
if exists('g:multi_cursor_next_key')
  exec 'nnoremap <silent> '.g:multi_cursor_next_key.
        \' :call multiple_cursors#new("n")<CR>'
  exec 'xnoremap <silent> '.g:multi_cursor_next_key.
        \' :<C-u>call multiple_cursors#new("v")<CR>'
endif
if exists('g:multi_cursor_prev_key')
  exec 'xnoremap <silent> '.g:multi_cursor_prev_key.
        \' :<C-u>call multiple_cursors#prev()<CR>'
endif
if exists('g:multi_cursor_skip_key')
  exec 'xnoremap <silent> '.g:multi_cursor_skip_key.
        \' :<C-u>call multiple_cursors#skip()<CR>'
endif

let &cpo = s:save_cpo
unlet s:save_cpo
