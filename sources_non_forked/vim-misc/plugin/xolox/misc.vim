" Vim plug-in
" Author: Peter Odding <peter@peterodding.com>
" Last Change: June 21, 2014
" URL: http://peterodding.com/code/vim/misc/

" Don't source the plug-in when it's already been loaded or &compatible is set.
if &cp || exists('g:loaded_xolox_misc')
  finish
endif

" Automatic commands used by the vim-misc plug-in.
augroup PluginXoloxMisc
  autocmd! CursorHold,CursorHoldI * call xolox#misc#cursorhold#autocmd()
augroup END

" Make sure the plug-in is only loaded once.
let g:loaded_xolox_misc = 1

" vim: ts=2 sw=2 et
