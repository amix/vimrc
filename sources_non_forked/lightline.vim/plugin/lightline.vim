" =============================================================================
" Filename: plugin/lightline.vim
" Author: itchyny
" License: MIT License
" Last Change: 2018/06/22 08:49:00.
" =============================================================================

if exists('g:loaded_lightline') || v:version < 700
  finish
endif
let g:loaded_lightline = 1

let s:save_cpo = &cpo
set cpo&vim

augroup lightline
  autocmd!
  autocmd WinEnter,BufWinEnter,FileType,SessionLoadPost * call lightline#update()
  autocmd SessionLoadPost * call lightline#highlight()
  autocmd ColorScheme * if !has('vim_starting') || expand('<amatch>') !=# 'macvim'
        \ | call lightline#update() | call lightline#highlight() | endif
  autocmd CursorMoved,BufUnload * call lightline#update_once()
augroup END

let &cpo = s:save_cpo
unlet s:save_cpo
