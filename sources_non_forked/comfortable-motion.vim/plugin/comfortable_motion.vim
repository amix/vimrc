"=============================================================================
" File: comfortable_motion.vim
" Author: Yuta Taniguchi
" Created: 2016-10-02
"=============================================================================

scriptencoding utf-8

if exists('g:loaded_comfortable_motion')
  finish
endif
let g:loaded_comfortable_motion = 1

let s:save_cpo = &cpo
set cpo&vim


if !exists('g:comfortable_motion_no_default_key_mappings') ||
\  !g:comfortable_motion_no_default_key_mappings
  nnoremap <silent> <C-d> :call comfortable_motion#flick(100)<CR>
  nnoremap <silent> <C-u> :call comfortable_motion#flick(-100)<CR>

  nnoremap <silent> <C-f> :call comfortable_motion#flick(200)<CR>
  nnoremap <silent> <C-b> :call comfortable_motion#flick(-200)<CR>
endif


let &cpo = s:save_cpo
unlet s:save_cpo
