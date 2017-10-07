" vim: tabstop=2 shiftwidth=2 softtabstop=2 expandtab
" DBGPavim: a remote debugger interface to the DBGp protocol
"
" Script Info and Documentation  {{{
"=============================================================================
"    Copyright: Copyright (C) 2012 Brook Hong
"      License:	The MIT License
"
"				Permission is hereby granted, free of charge, to any person obtaining
"				a copy of this software and associated documentation files
"				(the "Software"), to deal in the Software without restriction,
"				including without limitation the rights to use, copy, modify,
"				merge, publish, distribute, sublicense, and/or sell copies of the
"				Software, and to permit persons to whom the Software is furnished
"				to do so, subject to the following conditions:
"
"				The above copyright notice and this permission notice shall be included
"				in all copies or substantial portions of the Software.
"
"				THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"				OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"				MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"				IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"				CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"				TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"				SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" Name Of File: dbgpavim.vim, dbgpavim.py
"  Description: remote debugger interface to DBGp protocol
"               The DBGPavim originates from http://www.vim.org/scripts/script.php?script_id=1152, with a new enhanced debugger engine.
"
"               This file should reside in the plugins directory along
"               with dbgpavim.py and be automatically sourced.
"
"               By default, the script expects the debugging engine to connect
"               on port 9000. You can change this with the g:dbgPavimPort
"               variable by putting the following line your vimrc:
"
"                 let g:dbgPavimPort = 10001
"
"               where 10001 is the new port number you want the server to
"               connect to.
"
"               There are three maximum limits you can set referring to the
"               properties (variables) returned by the debugging engine.
"
"               g:dbgPavimMaxChildren (default 1024): The max number of array or
"               object children to initially retrieve per variable.
"               For example:
"
"                 let g:dbgPavimMaxChildren = 64
"
"               g:dbgPavimMaxData (default 1024 bytes): The max amount of
"               variable data to retrieve.
"               For example:
"
"                 let g:dbgPavimMaxData = 2048
"
"               g:dbgPavimMaxDepth (default 1): The maximum depth that the
"               debugger engine may return when sending arrays, hashs or
"               object structures to the IDE.
"               For example:
"
"                 let g:dbgPavimMaxDepth = 10
"
"               g:dbgPavimBreakAtEntry (default 0): Whether to break at entry,
"               if set it 0, the debugger engine will break only at
"               breakpoints.
"               For example:
"
"                 let g:dbgPavimBreakAtEntry = 1
"
"               g:dbgPavimPathMap (default []): Map local path to remote path
"               on server.
"               For example:
"
"                 let g:dbgPavimPathMap = [['D:/works/php','/var/www'],]
"
"               To enable debug from CLI
"
"                 php -dxdebug.remote_autostart=1 -dxdebug.remote_port=9000 test.php
"=============================================================================
" }}}
" Do not source this script when python is not compiled in.
if !has("python")
    finish
endif

" Load dbgpavim.py either from the same path where dbgpavim.vim is
let s:dbgpavim_py = expand("<sfile>:p:h")."/dbgpavim.py"
if filereadable(s:dbgpavim_py)
  exec 'pyfile '.s:dbgpavim_py
else
  call confirm('dbgpavim.vim: Unable to find '.s:dbgpavim_py.'. Place it in either your home vim directory or in the Vim runtime directory.', 'OK')
endif

if !exists('g:dbgPavimPort')
  let g:dbgPavimPort = 9100
endif
if !exists('g:dbgPavimMaxChildren')
  let g:dbgPavimMaxChildren = 1024
endif
if !exists('g:dbgPavimMaxData')
  let g:dbgPavimMaxData = 1024
endif
if !exists('g:dbgPavimMaxDepth')
  let g:dbgPavimMaxDepth = 1
endif
if !exists('g:dbgPavimBreakAtEntry')
  let g:dbgPavimBreakAtEntry = 0
endif
if !exists('g:dbgPavimOnce')
  let g:dbgPavimOnce = 1
endif
if !exists('g:dbgPavimPathMap')
  let g:dbgPavimPathMap = []
endif
if !exists('g:dbgPavimShowContext')
  let g:dbgPavimShowContext = 0
endif
if !exists('g:dbgPavimKeyRun')
  let g:dbgPavimKeyRun = '<F5>'
endif
if !exists('g:dbgPavimKeyQuit')
  let g:dbgPavimKeyQuit = '<F6>'
endif
if !exists('g:dbgPavimKeyToggleBae')
  let g:dbgPavimKeyToggleBae = '<F8>'
endif
if !exists('g:dbgPavimKeyToggleBp')
  let g:dbgPavimKeyToggleBp = '<F10>'
endif
if !exists('g:dbgPavimKeyHelp')
  let g:dbgPavimKeyHelp = '<F1>'
endif
if !exists('g:dbgPavimKeyStepInto')
  let g:dbgPavimKeyStepInto = '<F2>'
endif
if !exists('g:dbgPavimKeyStepOver')
  let g:dbgPavimKeyStepOver = '<F3>'
endif
if !exists('g:dbgPavimKeyStepOut')
  let g:dbgPavimKeyStepOut = '<F4>'
endif
if !exists('g:dbgPavimKeyEval')
  let g:dbgPavimKeyEval = '<F7>'
endif
if !exists('g:dbgPavimKeyRelayout')
  let g:dbgPavimKeyRelayout = '<F9>'
endif
if !exists('g:dbgPavimKeyContextGet')
  let g:dbgPavimKeyContextGet = '<F11>'
endif
if !exists('g:dbgPavimKeyPropertyGet')
  let g:dbgPavimKeyPropertyGet = '<F12>'
endif
if !exists('g:dbgPavimKeyLargeWindow')
  let g:dbgPavimKeyLargeWindow = '<leader>+'
endif
if !exists('g:dbgPavimKeySmallWindow')
  let g:dbgPavimKeySmallWindow = '<leader>-'
endif
if !exists('g:dbgPavimLang')
  let g:dbgPavimLang = ''
endif
exec 'nnoremap <silent> '.g:dbgPavimKeyRun.' :python dbgPavim.run()<cr>'
exec 'nnoremap <silent> '.g:dbgPavimKeyQuit.' :python dbgPavim.quit()<cr>'
exec 'nnoremap <silent> '.g:dbgPavimKeyToggleBae.' :call Bae()<cr>'
exec 'nnoremap <silent> '.g:dbgPavimKeyLargeWindow.' :call ResizeWindow("+")<cr>'
exec 'nnoremap <silent> '.g:dbgPavimKeySmallWindow.' :call ResizeWindow("-")<cr>'

exec 'autocmd FileType php,python,javascript nnoremap <buffer> <silent> '.g:dbgPavimKeyToggleBp.' :python dbgPavim.mark()<cr>'

command! -nargs=? Bp python dbgPavim.mark('<args>')
command! -nargs=0 Bl python dbgPavim.list()
command! -nargs=0 Bc python dbgPavim.clear()
command! -nargs=0 Bu python dbgPavim.unclear()
command! -nargs=? Dp python dbgPavim.cli('<args>')
command! -nargs=? Wc python dbgPavim.watch("<args>")
command! -nargs=? We python dbgPavim.eval("<args>")
command! -nargs=0 Wl python dbgPavim.listWatch()
command! -nargs=1 Children let g:dbgPavimMaxChildren=<args>|python dbgPavim.setMaxChildren()
command! -nargs=1 Depth let g:dbgPavimMaxDepth=<args>|python dbgPavim.setMaxDepth()
command! -nargs=1 Length let g:dbgPavimMaxData=<args>|python dbgPavim.setMaxData()

let s:keyMappings = {
      \ g:dbgPavimKeyHelp : ':python dbgPavim.ui.help()<cr>',
      \ g:dbgPavimKeyStepInto : ':python dbgPavim.command(\"step_into\")<cr>',
      \ g:dbgPavimKeyStepOver : ':python dbgPavim.command(\"step_over\")<cr>',
      \ g:dbgPavimKeyStepOut : ':python dbgPavim.command(\"step_out\")<cr>',
      \ g:dbgPavimKeyEval : ':python dbgPavim.watch_input(\"eval\")<cr>A',
      \ g:dbgPavimKeyRelayout : ':python dbgPavim.ui.reLayout()<cr>',
      \ g:dbgPavimKeyContextGet : ':python dbgPavim.context()<cr>',
      \ g:dbgPavimKeyPropertyGet : ':python dbgPavim.property()<cr>',
      \ }
for key in keys(s:keyMappings)
  exec 'nnoremap <expr> <silent> '.key.' (exists("g:dbgPavimTab")==1 && g:dbgPavimTab == tabpagenr() ? "'.s:keyMappings[key].'" : "'.key.'")'
endfor
exec 'vnoremap '.g:dbgPavimKeyPropertyGet.' "vy:python dbgPavim.property("%v%")<CR>'
exec 'vnoremap '.g:dbgPavimKeyEval.' "vy:python dbgPavim.watch_input("eval", "%v%")<CR>$a<CR>'
command! -nargs=0 Up python dbgPavim.up()
command! -nargs=0 Dn python dbgPavim.down()
command! -nargs=? Pg python dbgPavim.property("<args>")

function! ResizeWindow(flag)
  let l:width = winwidth("%")
  if l:width == &columns
    execute 'resize '.a:flag.'5'
  else
    execute 'vertical resize '.a:flag.'5'
  endif
endfunction
function! Bae()
  let g:dbgPavimBreakAtEntry = (g:dbgPavimBreakAtEntry == 1) ? 0 : 1
  execute 'python dbgPavim.breakAtEntry = '.g:dbgPavimBreakAtEntry
endfunction
function! WatchWindowOnEnter()
  let l:line = getline(".")
  if l:line =~ "^\\s*.* = (.*)+;$"
    let l:var = substitute(line,"\\s*\\(\\S.*\\S\\)\\s*=.*","\\1","g")
    let l:var = substitute(l:var,"'","\\\\'","g")
    execute "python dbgPavim.debugSession.expandVar('".l:var."')"
    execute "normal \<c-w>p"
  elseif l:line =~ "^\\d\\+  .*:\\d\\+$"
    let fn = substitute(l:line,"^\\d\\+  \\(.*\\):\\d\\+$","\\1","")
    let ln = substitute(l:line,"^\\d\\+  .*:\\(\\d\\+\\)$","\\1","")
    execute 'python dbgPavim.debugSession.jump("'.l:fn.'",'.l:ln.')'
  elseif foldlevel(".") > 0
    execute 'normal za'
  endif
endfunction
function! StackWindowOnEnter()
  let l:stackNo = substitute(getline("."),"\\(\\d\\+\\)\\s\\+.*","\\1","g")
  if l:stackNo =~ "^\\d\\+$"
    execute 'python dbgPavim.debugSession.go('.l:stackNo.')'
    execute "normal \<c-w>p"
  endif
endfunction

function! CheckPydbgp()
  let l:ret = 0
  let l:pydbgp = expand('`pydbgp -h`')
  if l:pydbgp == ''
    echo "Please install Komodo Python Remote Debugging Client."
    let l:ret = 1
  endif
  return l:ret
endfunction

function! CheckXdebug()
  let l:ret = 0
  let l:phpinfo = system('php -r "phpinfo();"')
  let l:port = matchstr(l:phpinfo, 'xdebug.remote_port => \d\+')
  let l:handler = matchstr(l:phpinfo, 'xdebug.remote_handler => \a\+')
  let l:enable = matchstr(l:phpinfo, 'xdebug.remote_enable => \a\+')
  if l:handler !='xdebug.remote_handler => dbgp'
    echo l:handler.' inconsistent with dbgp'
    let l:ret = 1
  endif
  if l:enable !='xdebug.remote_enable => On'
    echo l:enable.' inconsistent with On'
    let l:ret = 1
  endif
  return l:ret
endfunction
function! Signs()
  let l:signs = ''
  redir => l:signs
  silent exec 'sign place buffer='.bufnr('%')
  redir END
  let l:bpts = {}
  let l:lines = split(l:signs, '\n')
  for l:line in l:lines
    if l:line =~ "\\S*:$"
      let l:file = expand("%:p")
    elseif l:line =~ "^\\s*\\S*=\\d*\\s*\\S*=\\d*\\s*\\S*=breakpt$"
      let l:lno = substitute(l:line,"^\\s*\\S*=\\(\\d\\+\\)\\s*\\S*=\\d*\\s*\\S*=breakpt$", "\\1", "g")
      let l:id = substitute(l:line,"^\\s*\\S*=\\d\\+\\s*\\S*=\\(\\d\\+\\)\\s*\\S*=breakpt$", "\\1", "g")
      let l:bpts[l:id] = [l:file, l:lno]
    endif
  endfor
  return l:bpts
endfunction

if !hlexists('DbgCurrent')
  hi DbgCurrent term=reverse ctermfg=White ctermbg=Red gui=reverse
endif
if !hlexists('DbgBreakPt')
  hi DbgBreakPt term=reverse ctermfg=White ctermbg=Green gui=reverse
endif
sign define current text=->  texthl=DbgCurrent linehl=DbgCurrent
sign define breakpt text=B>  texthl=DbgBreakPt linehl=DbgBreakPt

set laststatus=2
python dbgPavim_init()

autocmd BufEnter WATCH_WINDOW nnoremap <silent> <buffer> <Enter> :call WatchWindowOnEnter()<CR>
autocmd BufEnter STACK_WINDOW nnoremap <silent> <buffer> <Enter> :call StackWindowOnEnter()<CR>
autocmd BufLeave HELP__WINDOW :python dbgPavim.ui.helpwin=None
autocmd VimLeavePre * python dbgPavim.quit()
