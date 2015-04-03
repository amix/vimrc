" AlignPlugin: tool to align multiple fields based on one or more separators
"   Author:	 Charles E. Campbell
"   Date:    Nov 02, 2008
" GetLatestVimScripts: 294 1 :AutoInstall: Align.vim
" GetLatestVimScripts: 1066 1 :AutoInstall: cecutil.vim
" Copyright:    Copyright (C) 1999-2012 Charles E. Campbell {{{1
"               Permission is hereby granted to use and distribute this code,
"               with or without modifications, provided that this copyright
"               notice is copied with it. Like anything else that's free,
"               Align.vim is provided *as is* and comes with no warranty
"               of any kind, either expressed or implied. By using this
"               plugin, you agree that in no event will the copyright
"               holder be liable for any damages resulting from the use
"               of this software.
"
" Romans 1:16,17a : For I am not ashamed of the gospel of Christ, for it is {{{1
" the power of God for salvation for everyone who believes; for the Jew first,
" and also for the Greek.  For in it is revealed God's righteousness from
" faith to faith.
" ---------------------------------------------------------------------
" Load Once: {{{1
if &cp || exists("g:loaded_AlignPlugin")
 finish
endif
let g:loaded_AlignPlugin = "v37"
let s:keepcpo            = &cpo
set cpo&vim

" ---------------------------------------------------------------------
" Public Interface: {{{1
com! -bang -range -nargs=* Align <line1>,<line2>call Align#Align(<bang>0,<q-args>)
com!       -range -nargs=0 AlignReplaceQuotedSpaces <line1>,<line2>call Align#AlignReplaceQuotedSpaces()
com!              -nargs=* AlignCtrl call Align#AlignCtrl(<q-args>)
com!              -nargs=0 AlignPush call Align#AlignPush()
com!              -nargs=0 AlignPop  call Align#AlignPop()

" ---------------------------------------------------------------------
"  Restore: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo
" vim: ts=4 fdm=marker
