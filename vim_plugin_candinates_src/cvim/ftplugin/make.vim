" ------------------------------------------------------------------------------
"
" Vim filetype plugin file (part of the c.vim plugin)
"
"   Language :  make 
"     Plugin :  c.vim 
" Maintainer :  Fritz Mehner <mehner@fh-swf.de>
"   Revision :  $Id: make.vim,v 1.4 2011/12/27 21:04:33 mehner Exp $
"
" ------------------------------------------------------------------------------
"
" Only do this when not done yet for this buffer
" 
if exists("b:did_make_ftplugin")
  finish
endif
let b:did_make_ftplugin = 1

 map    <buffer>  <silent>  <C-F9>                  :call C_Make()<CR>
imap    <buffer>  <silent>  <C-F9>             <C-C>:call C_Make()<CR>
 map    <buffer>  <silent>  <LocalLeader>rm         :call C_Make()<CR>
imap    <buffer>  <silent>  <LocalLeader>rm    <C-C>:call C_Make()<CR>
 map    <buffer>  <silent>  <LocalLeader>rcm        :call C_ChooseMakefile()<CR>
imap    <buffer>  <silent>  <LocalLeader>rcm   <C-C>:call C_ChooseMakefile()<CR>
 map    <buffer>  <silent>  <LocalLeader>rmc        :call C_MakeClean()<CR>
imap    <buffer>  <silent>  <LocalLeader>rmc   <C-C>:call C_MakeClean()<CR>
 map    <buffer>  <silent>  <LocalLeader>rme        :call C_MakeExeToRun()<CR>
imap    <buffer>  <silent>  <LocalLeader>rme   <C-C>:call C_MakeExeToRun()<CR>
 map    <buffer>  <silent>  <LocalLeader>rma        :call C_MakeArguments()<CR>
imap    <buffer>  <silent>  <LocalLeader>rma   <C-C>:call C_MakeArguments()<CR>

