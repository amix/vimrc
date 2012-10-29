" genutils.vim: Please see plugin/genutils.vim
"
" TODO:
"   - fnamemodify() on Unix doesn't expand to full name if the filename doesn't
"     really exist on the filesystem.
"   - Is setting 'scrolloff' and 'sidescrolloff' to 0 required while moving the
"     cursor?
"
"   - EscapeCommand() didn't work for David Fishburn.
"   - Save/RestoreWindowSettings doesn't work well.
"
"   Vim7:
"   - Save/RestoreWindowSettings can use winsave/restview() functions.
"

" Make sure line-continuations won't cause any problem. This will be restored
"   at the end
let s:save_cpo = &cpo
set cpo&vim


let g:makeArgumentString = 'exec genutils#MakeArgumentString()'
let g:makeArgumentList = 'exec genutils#MakeArgumentList()'

let s:makeArgumentString = ''
function! genutils#MakeArgumentString(...)
  if s:makeArgumentString == ''
    let s:makeArgumentString = genutils#ExtractFuncListing(s:SNR().
          \ '_makeArgumentString', 0, 0)
  endif
  if a:0 > 0 && a:1 != ''
    return substitute(s:makeArgumentString, '\<argumentString\>', a:1, 'g')
  else
    return s:makeArgumentString
  endif
endfunction


let s:makeArgumentList = ''
function! genutils#MakeArgumentList(...)
  if s:makeArgumentList == ''
    let s:makeArgumentList = genutils#ExtractFuncListing(s:SNR().
          \ '_makeArgumentList', 0, 0)
  endif
  if a:0 > 0 && a:1 != ''
    let mkArgLst = substitute(s:makeArgumentList, '\<argumentList\>', a:1, 'g')
    if a:0 > 1 && a:2 != ''
      let mkArgLst = substitute(s:makeArgumentList,
            \ '\(\s\+let __argSeparator = \)[^'."\n".']*', "\\1'".a:2."'", '')
    endif
    return mkArgLst
  else
    return s:makeArgumentList
  endif
endfunction

function! genutils#ExtractFuncListing(funcName, hLines, tLines)
  let listing = genutils#GetVimCmdOutput('func '.a:funcName)
  let listing = substitute(listing,
        \ '^\%(\s\|'."\n".'\)*function '.a:funcName.'([^)]*)'."\n", '', '')
  "let listing = substitute(listing, '\%(\s\|'."\n".'\)*endfunction\%(\s\|'."\n".'\)*$', '', '')
  " Leave the last newline character.
  let listing = substitute(listing, '\%('."\n".'\)\@<=\s*endfunction\s*$', '', '')
  let listing = substitute(listing, '\(\%(^\|'."\n".'\)\s*\)\@<=\d\+',
        \ '', 'g')
  if a:hLines > 0
    let listing = substitute(listing, '^\%([^'."\n".']*'."\n".'\)\{'.
          \ a:hLines.'}', '', '')
  endif
  if a:tLines > 0
    let listing = substitute(listing, '\%([^'."\n".']*'."\n".'\)\{'.
          \ a:tLines.'}$', '', '')
  endif
  return listing
endfunction

function! genutils#CreateArgString(argList, sep, ...)
  let sep = (a:0 == 0) ? a:sep : a:1 " This is no longer used.
  " Matching multvals functionality means, we need to ignore the trailing
  " separator.
  let argList = split(substitute(a:argList, a:sep.'$', '', ''), a:sep, 1)
  let argString = "'"
  for nextArg in argList
    " FIXME: I think this check is not required. If "'" is the separator, we
    "   don't expect to see them in the elements.
    if a:sep != "'"
      let nextArg = substitute(nextArg, "'", "' . \"'\" . '", 'g')
    endif
    let argString = argString . nextArg . "', '"
  endfor
  let argString = strpart(argString, 0, strlen(argString) - 3)
  return argString
endfunction

" {{{
function! s:_makeArgumentString()
  let __argCounter = 1
  let argumentString = ''
  while __argCounter <= a:0
    if type(a:{__argCounter})
      let __nextArg =  "'" .
            \ substitute(a:{__argCounter}, "'", "' . \"'\" . '", "g") . "'"
    else
      let __nextArg = a:{__argCounter}
    endif
    let argumentString = argumentString. __nextArg .
          \ ((__argCounter == a:0) ? '' : ', ')
    let __argCounter = __argCounter + 1
  endwhile
  unlet __argCounter
  if exists('__nextArg')
    unlet __nextArg
  endif
endfunction

function! s:_makeArgumentList()
  let __argCounter = 1
  let __argSeparator = ','
  let argumentList = ''
  while __argCounter <= a:0
    let argumentList = argumentList . a:{__argCounter}
    if __argCounter != a:0
      let argumentList = argumentList . __argSeparator
    endif
    let __argCounter = __argCounter + 1
  endwhile
  unlet __argCounter
  unlet __argSeparator
endfunction
" }}}


function! genutils#DebugShowArgs(...)
  let i = 0
  let argString = ''
  while i < a:0
    let argString = argString . a:{i + 1} . ', '
    let i = i + 1
  endwhile
  let argString = strpart(argString, 0, strlen(argString) - 2)
  call input("Args: " . argString)
endfunction

" Window related functions {{{

function! genutils#NumberOfWindows()
  let i = 1
  while winbufnr(i) != -1
    let i = i+1
  endwhile
  return i - 1
endfunction

" Find the window number for the buffer passed.
" The fileName argument is treated literally, unlike the bufnr() which treats
"   the argument as a regex pattern.
function! genutils#FindWindowForBuffer(bufferName, checkUnlisted)
  return bufwinnr(genutils#FindBufferForName(a:bufferName))
endfunction

function! genutils#FindBufferForName(fileName)
  " The name could be having extra backslashes to protect certain chars (such
  "   as '#' and '%'), so first expand them.
  return s:FindBufferForName(genutils#UnEscape(a:fileName, '#%'))
endfunction

function! s:FindBufferForName(fileName)
  let fileName = genutils#Escape(a:fileName, '[?,{')
  let _isf = &isfname
  try
    set isfname-=\
    set isfname-=[
    let i = bufnr('^' . fileName . '$')
  finally
    let &isfname = _isf
  endtry
  return i
endfunction

function! genutils#GetBufNameForAu(bufName)
  let bufName = a:bufName
  " Autocommands always require forward-slashes.
  let bufName = substitute(bufName, "\\\\", '/', 'g')
  let bufName = escape(bufName, '*?,{}[ ')
  return bufName
endfunction

function! genutils#MoveCursorToWindow(winno)
  if genutils#NumberOfWindows() != 1
    execute a:winno . " wincmd w"
  endif
endfunction

function! genutils#MoveCurLineToWinLine(n)
  normal! zt
  if a:n == 1
    return
  endif
  let _wrap = &l:wrap
  setl nowrap
  let n = a:n
  if n >= winheight(0)
    let n = winheight(0)
  endif
  let n = n - 1
  execute "normal! " . n . "\<C-Y>"
  let &l:wrap = _wrap
endfunction

function! genutils#CloseWindow(win, force)
  let _eventignore = &eventignore
  try
    set eventignore=all
    call genutils#MarkActiveWindow()

    let &eventignore = _eventignore
    exec a:win 'wincmd w'
    exec 'close'.(a:force ? '!' : '')
    set eventignore=all

    if a:win < t:curWinnr
      let t:curWinnr = t:curWinnr - 1
    endif
    if a:win < t:prevWinnr
      let t:prevWinnr = t:prevWinnr - 1
    endif
  finally
    call genutils#RestoreActiveWindow()
    let &eventignore = _eventignore
  endtry
endfunction

function! genutils#MarkActiveWindow()
  let t:curWinnr = winnr()
  " We need to restore the previous-window also at the end.
  silent! wincmd p
  let t:prevWinnr = winnr()
  silent! wincmd p
endfunction

function! genutils#RestoreActiveWindow()
  if !exists('t:curWinnr')
    return
  endif

  " Restore the original window.
  if winnr() != t:curWinnr
    exec t:curWinnr'wincmd w'
  endif
  if t:curWinnr != t:prevWinnr
    exec t:prevWinnr'wincmd w'
    wincmd p
  endif
endfunction

function! genutils#IsOnlyVerticalWindow()
  let onlyVertWin = 1
  let _eventignore = &eventignore

  try
    "set eventignore+=WinEnter,WinLeave
    set eventignore=all
    call genutils#MarkActiveWindow()

    wincmd j
    if winnr() != t:curWinnr
      let onlyVertWin = 0
    else
      wincmd k
      if winnr() != t:curWinnr
	let onlyVertWin = 0
      endif
    endif
  finally
    call genutils#RestoreActiveWindow()
    let &eventignore = _eventignore
  endtry
  return onlyVertWin
endfunction

function! genutils#IsOnlyHorizontalWindow()
  let onlyHorizWin = 1
  let _eventignore = &eventignore
  try
    set eventignore=all
    call genutils#MarkActiveWindow()
    wincmd l
    if winnr() != t:curWinnr
      let onlyHorizWin = 0
    else
      wincmd h
      if winnr() != t:curWinnr
	let onlyHorizWin = 0
      endif
    endif
  finally
    call genutils#RestoreActiveWindow()
    let &eventignore = _eventignore
  endtry
  return onlyHorizWin
endfunction

function! genutils#MoveCursorToNextInWinStack(dir)
  let newwin = genutils#GetNextWinnrInStack(a:dir)
  if newwin != 0
    exec newwin 'wincmd w'
  endif
endfunction

function! genutils#GetNextWinnrInStack(dir)
  let newwin = winnr()
  let _eventignore = &eventignore
  try
    set eventignore=all
    call genutils#MarkActiveWindow()
    let newwin = s:GetNextWinnrInStack(a:dir)
  finally
    call genutils#RestoreActiveWindow()
    let &eventignore = _eventignore
  endtry
  return newwin
endfunction

function! genutils#MoveCursorToLastInWinStack(dir)
  let newwin = genutils#GetLastWinnrInStack(a:dir)
  if newwin != 0
    exec newwin 'wincmd w'
  endif
endfunction

function! genutils#GetLastWinnrInStack(dir)
  let newwin = winnr()
  let _eventignore = &eventignore
  try
    set eventignore=all
    call genutils#MarkActiveWindow()
    while 1
      let wn = s:GetNextWinnrInStack(a:dir)
      if wn != 0
        let newwin = wn
        exec newwin 'wincmd w'
      else
        break
      endif
    endwhile
  finally
    call genutils#RestoreActiveWindow()
    let &eventignore = _eventignore
  endtry
  return newwin
endfunction

" Based on the WinStackMv() function posted by Charles E. Campbell, Jr. on vim
"   mailing list on Jul 14, 2004.
function! s:GetNextWinnrInStack(dir)
  "call Decho("genutils#MoveCursorToNextInWinStack(dir<".a:dir.">)")

  let isHorizontalMov = (a:dir ==# 'h' || a:dir ==# 'l') ? 1 : 0

  let orgwin = winnr()
  let orgdim = s:GetWinDim(a:dir, orgwin)

  let _winwidth = &winwidth
  let _winheight = &winheight
  try
    set winwidth=1
    set winheight=1
    exec 'wincmd' a:dir
    let newwin = winnr()
    if orgwin == newwin
      " No more windows in this direction.
      "call Decho("newwin=".newwin." stopped".winheight(newwin)."x".winwidth(newwin))
      return 0
    endif
    if s:GetWinDim(a:dir, newwin) != orgdim
      " Window dimension has changed, indicates a move across window stacks.
      "call Decho("newwin=".newwin." height changed".winheight(newwin)."x".winwidth(newwin))
      return 0
    endif
    " Determine if changing original window height affects current window
    "   height.
    exec orgwin 'wincmd w'
    try
      if orgdim == 1
        exec 'wincmd' (isHorizontalMov ? '_' : '|')
      else
        exec 'wincmd' (isHorizontalMov ? '-' : '<')
      endif
      if s:GetWinDim(a:dir, newwin) != s:GetWinDim(a:dir, orgwin)
        "call Decho("newwin=".newwin." different row".winheight(newwin)."x".winwidth(newwin))
        return 0
      endif
      "call Decho("newwin=".newwin." same row".winheight(newwin)."x".winwidth(newwin))
    finally
      exec (isHorizontalMov ? '' : 'vert') 'resize' orgdim
    endtry

    "call Decho("genutils#MoveCursorToNextInWinStack")

    return newwin
  finally
    let &winwidth = _winwidth
    let &winheight = _winheight
  endtry
endfunction

function! s:GetWinDim(dir, win)
  return (a:dir ==# 'h' || a:dir ==# 'l') ? winheight(a:win) : winwidth(a:win)
endfunction

function! genutils#OpenWinNoEa(winOpenCmd)
  call s:ExecWinCmdNoEa(a:winOpenCmd)
endfunction

function! genutils#CloseWinNoEa(winnr, force)
  call s:ExecWinCmdNoEa(a:winnr.'wincmd w | close'.(a:force?'!':''))
endfunction

function! s:ExecWinCmdNoEa(winCmd)
  let _eventignore = &eventignore
  try
    set eventignore=all
    call genutils#MarkActiveWindow()
    windo let w:_winfixheight = &winfixheight
    windo set winfixheight
    call genutils#RestoreActiveWindow()

    let &eventignore = _eventignore
    exec a:winCmd
    set eventignore=all

    call genutils#MarkActiveWindow()
    silent! windo let &winfixheight = w:_winfixheight
    silent! windo unlet w:_winfixheight
    call genutils#RestoreActiveWindow()
  finally
    let &eventignore = _eventignore
  endtry
endfunction

" Window related functions }}}

function! genutils#SetupScratchBuffer()
  setlocal nobuflisted
  setlocal noswapfile
  setlocal buftype=nofile
  " Just in case, this will make sure we are always hidden.
  setlocal bufhidden=delete
  setlocal nolist
  setlocal nonumber
  setlocal foldcolumn=0 nofoldenable
  setlocal noreadonly
endfunction

function! genutils#CleanDiffOptions()
  setlocal nodiff
  setlocal noscrollbind
  setlocal scrollopt-=hor
  setlocal wrap
  setlocal foldmethod=manual
  setlocal foldcolumn=0
  normal zE
endfunction

function! genutils#ArrayVarExists(varName, index)
  try
    exec "let test = " . a:varName . "{a:index}"
  catch /^Vim\%((\a\+)\)\=:E121/
    return 0
  endtry
  return 1
endfunction

function! genutils#Escape(str, chars)
  return substitute(a:str, '\\\@<!\(\%(\\\\\)*\)\([' . a:chars .']\)', '\1\\\2',
        \ 'g')
endfunction

function! genutils#UnEscape(str, chars)
  return substitute(a:str, '\\\@<!\(\\\\\)*\\\([' . a:chars . ']\)',
        \ '\1\2', 'g')
endfunction

function! genutils#DeEscape(str)
  let str = a:str
  let str = substitute(str, '\\\(\\\|[^\\]\)', '\1', 'g')
  return str
endfunction

" - For windoze+native, use double-quotes to sorround the arguments and for
"   embedded double-quotes, just double them.
" - For windoze+sh, use single-quotes to sorround the aruments and for embedded
"   single-quotes, just replace them with '""'""' (if 'shq' or 'sxq' is a
"   double-quote) and just '"'"' otherwise. Embedded double-quotes also need
"   to be doubled.
" - For Unix+sh, use single-quotes to sorround the arguments and for embedded
"   single-quotes, just replace them with '"'"'. 
function! genutils#EscapeCommand(cmd, args, pipe)
  if type(a:args) == 3
    let args = copy(a:args)
  else
    let args = split(a:args, genutils#CrUnProtectedCharsPattern(' '))
  endif
  " I am only worried about passing arguments with spaces as they are to the
  "   external commands, I currently don't care about back-slashes
  "   (backslashes are normally expected only on windows when 'shellslash'
  "   option is set, but even then the 'shell' is expected to take care of
  "   them.). However, for cygwin bash, there is a loss of one level
  "   of the back-slashes somewhere in the chain of execution (most probably
  "   between CreateProcess() and cygwin?), so we need to double them.
  let shellEnvType = genutils#GetShellEnvType()
  if shellEnvType ==# g:ST_WIN_CMD
    let quoteChar = '"'
    " genutils#Escape the existing double-quotes (by doubling them).
    call map(args, "substitute(v:val, '\"', '\"\"', 'g')")
  else
    let quoteChar = "'"
    if shellEnvType ==# g:ST_WIN_SH
      " genutils#Escape the existing double-quotes (by doubling them).
      call map(args, "substitute(v:val, '\"', '\"\"', 'g')")
    endif
    " Take care of existing single-quotes (by exposing them, as you can't have
    "   single-quotes inside a single-quoted string).
    if &shellquote == '"' || &shellxquote == '"'
      let squoteRepl = "'\"\"'\"\"'"
    else
      let squoteRepl = "'\"'\"'"
    endif
    call map(args, "substitute(v:val, \"'\", squoteRepl, 'g')")
  endif

  " Now sorround the arguments with quotes, considering the protected
  "   spaces. Skip the && or || construct from doing this.
  call map(args, 'v:val=~"^[&|]\\{2}$"?(v:val):(quoteChar.v:val.quoteChar)')
  let fullCmd = join(args, ' ')
  " We delay adding pipe part so that we can avoid the above processing.
  let pipe = ''
  if type(a:pipe) == 3 && len(a:pipe) > 0
    let pipe = join(a:pipe, ' ')
  elseif type(a:pipe) == 1 && a:pipe !~# '^\s*$'
    let pipe = a:pipe
  endif
  if pipe != ''
    let fullCmd = fullCmd . ' ' . a:pipe
  endif
  if a:cmd !~# '^\s*$'
    let fullCmd = a:cmd . ' ' . fullCmd
  endif
  if shellEnvType ==# g:ST_WIN_SH && &shell =~# '\<bash\>'
    let fullCmd = substitute(fullCmd, '\\', '\\\\', 'g')
  endif
  return fullCmd
endfunction

let g:ST_WIN_CMD = 0 | let g:ST_WIN_SH = 1 | let g:ST_UNIX = 2
function! genutils#GetShellEnvType()
  " When 'shellslash' option is available, then the platform must be one of
  "     those that support '\' as a pathsep.
  if exists('+shellslash')
    if stridx(&shell, 'cmd.exe') != -1 ||
          \ stridx(&shell, 'command.com') != -1
      return g:ST_WIN_CMD
    else
      return g:ST_WIN_SH
    endif
  else
    return g:ST_UNIX
  endif
endfunction

function! genutils#ExpandStr(str)
  let str = substitute(a:str, '"', '\\"', 'g')
  exec "let str = \"" . str . "\"" 
  return str
endfunction

function! genutils#QuoteStr(str)
  return "'".substitute(a:str, "'", "'.\"'\".'", 'g')."'"
endfunction

function! genutils#GetPreviewWinnr()
  let _eventignore = &eventignore
  let curWinNr = winnr()
  let winnr = -1
  try
    set eventignore=all
    exec "wincmd P"
    let winnr = winnr()
  catch /^Vim\%((\a\+)\)\=:E441/
    " Ignore, winnr is already set to -1.
  finally
    if winnr() != curWinNr
      exec curWinNr.'wincmd w'
    endif
    let &eventignore = _eventignore
  endtry
  return winnr
endfunction

" Save/Restore window settings {{{
function! genutils#SaveWindowSettings()
  call genutils#SaveWindowSettings2('SaveWindowSettings', 1)
endfunction

function! genutils#RestoreWindowSettings()
  call genutils#RestoreWindowSettings2('SaveWindowSettings')
endfunction


function! genutils#ResetWindowSettings()
  call genutils#ResetWindowSettings2('SaveWindowSettings')
endfunction

function! genutils#SaveWindowSettings2(id, overwrite)
  if genutils#ArrayVarExists("t:winSettings", a:id) && ! a:overwrite
    return
  endif

  let t:winSettings{a:id} = []
  call add(t:winSettings{a:id}, genutils#NumberOfWindows())
  call add(t:winSettings{a:id}, &lines)
  call add(t:winSettings{a:id}, &columns)
  call add(t:winSettings{a:id}, winnr())
  let i = 1
  while winbufnr(i) != -1
    call add(t:winSettings{a:id}, winheight(i))
    call add(t:winSettings{a:id}, winwidth(i))
    let i = i + 1
  endwhile
  "let g:savedWindowSettings = t:winSettings{a:id} " Debug.
endfunction

function! genutils#RestoreWindowSettings2(id)
  " Calling twice fixes most of the resizing issues. This seems to be how the
  " :mksession with "winsize" in 'sesionoptions' seems to work.
  call s:RestoreWindowSettings2(a:id)
  call s:RestoreWindowSettings2(a:id)
endfunction

function! s:RestoreWindowSettings2(id)
  "if ! exists("t:winSettings" . a:id)
  if ! genutils#ArrayVarExists("t:winSettings", a:id)
    return
  endif

  let nWindows = t:winSettings{a:id}[0]
  if nWindows != genutils#NumberOfWindows()
    unlet t:winSettings{a:id}
    return
  endif
  let orgLines = t:winSettings{a:id}[1]
  let orgCols = t:winSettings{a:id}[2]
  let activeWindow = t:winSettings{a:id}[3]
  let mainWinSizeSame = (orgLines == &lines && orgCols == &columns)

  let winNo = 1
  let i = 4
  while i < len(t:winSettings{a:id})
    let height = t:winSettings{a:id}[i]
    let width = t:winSettings{a:id}[i+1]
    let height = (mainWinSizeSame ? height :
          \ ((&lines * height + (orgLines / 2)) / orgLines))
    let width = (mainWinSizeSame ? width :
          \ ((&columns * width + (orgCols / 2)) / orgCols))
    if winheight(winnr()) != height
      exec winNo'resize' height
    endif
    if winwidth(winnr()) != width
      exec 'vert' winNo 'resize' width
    endif
    let winNo = winNo + 1
    let i = i + 2
  endwhile
  
  " Restore the current window.
  call genutils#MoveCursorToWindow(activeWindow)
  "unlet g:savedWindowSettings
endfunction


function! genutils#ResetWindowSettings2(id)
  if genutils#ArrayVarExists("t:winSettings", a:id)
    unlet t:winSettings{a:id}
  endif
endfunction

" Save/Restore window settings }}}

" Save/Restore selection {{{

function! genutils#SaveVisualSelection(id)
  let curmode = mode()
  if curmode == 'n'
    normal! gv
  endif
  let s:vismode{a:id} = mode()
  let s:firstline{a:id} = line("'<")
  let s:lastline{a:id} = line("'>")
  let s:firstcol{a:id} = col("'<")
  let s:lastcol{a:id} = col("'>")
  if curmode !=# s:vismode{a:id}
    exec "normal \<Esc>"
  endif
endfunction

function! genutils#RestoreVisualSelection(id)
  if mode() !=# 'n'
    exec "normal \<Esc>"
  endif
  if exists('s:vismode{id}')
    exec 'normal' s:firstline{a:id}.'gg'.s:firstcol{a:id}.'|'.
          \ s:vismode{a:id}.(s:lastline{a:id}-s:firstline{a:id}).'j'.
          \ (s:lastcol{a:id}-s:firstcol{a:id}).'l'
  endif
endfunction
" Save/Restore selection }}}

function! genutils#CleanupFileName(fileName)
  return genutils#CleanupFileName2(a:fileName, '')
endfunction

function! genutils#CleanupFileName2(fileName, win32ProtectedChars)
  let fileName = expand(substitute(a:fileName, '^\s\+\|\s\+$', '', 'g'))

  if genutils#OnMS() && (match(fileName, '^//') == 0 ||
        \ match(fileName, '^\\\\') == 0)
    let uncPath = 1
  else
    let uncPath = 0
  endif

  " Remove multiple path separators.
  if has('win32')
    if a:win32ProtectedChars != ''
      let fileName=substitute(fileName, '\\['.a:win32ProtectedChars.']\@!', '/',
            \ 'g')
    else
      let fileName=substitute(fileName, '\\', '/', 'g')
    endif
  elseif genutils#OnMS()
    " On non-win32 systems, the forward-slash is not supported, so leave
    " back-slash.
    let fileName=substitute(fileName, '\\\{2,}', '\', 'g')
  endif
  let fileName=substitute(fileName, '/\{2,}', '/', 'g')

  " If it was an UNC path, add back an extra slash.
  if uncPath
    let fileName = '/'.fileName
  else
    " Expand relative paths and paths containing relative components (takes care
    " of ~ also). This also adds the drive letter if it is missing. Special
    " case when drive root is used with no trailing slash (e.g., c:), don't
    " expand, Vim replaces it with previous directory on that drive. Also
    " don't leave any trailing slashes, as they appear conditional to whether
    " the path is an existing dir or not.
    let fileName = (genutils#OnMS() && fileName =~# '^[a-z]:$') ? fileName.'/' :
          \ substitute(fnamemodify(fileName, ':p'), '\(.\)[\\/]$', '\1', '')
  endif

  if genutils#OnMS()
    let fileName=substitute(fileName, '^[A-Z]:', '\L&', '')
  endif
  return fileName
endfunction
"echo genutils#CleanupFileName('\\a///b/c\') " //a/b/c
"echo genutils#CleanupFileName('C:\a/b/c\d') " c:/a/b/c/d
"echo genutils#CleanupFileName('a/b/c\d') " z:/hari/vimfiles/a/b/c/d
"echo genutils#CleanupFileName('~/a/b/c\d') " z:/hari/a/b/c/d
"echo genutils#CleanupFileName('~/a/b/../c\d') " z:/hari/a/b/c/d
"echo genutils#CleanupFileName('') " z:/hari/vimfiles
"echo genutils#CleanupFileName('/') " z:/
"echo genutils#CleanupFileName('z:') " z:/
"echo genutils#CleanupFileName('z:/') " z:/
"echo genutils#CleanupFileName('C:/windows/') " c:/windows

function! genutils#OnMS()
  return has('win32') || has('dos32') || has('win16') || has('dos16') ||
       \ has('win95')
endfunction

function! genutils#PathIsAbsolute(path)
  let absolute=0
  let path = expand(a:path)
  if has('unix') || genutils#OnMS()
    if match(path, '^/') == 0
      let absolute=1
    endif
  endif
  if (! absolute) && genutils#OnMS()
    if match(path, "^\\") == 0
      let absolute=1
    endif
  endif
  if (! absolute) && genutils#OnMS()
    if match(path, "^[A-Za-z]:") == 0
      let absolute=1
    endif
  endif
  return absolute
endfunction

function! genutils#PathIsFileNameOnly(path)
  return (match(a:path, "\\") < 0) && (match(a:path, "/") < 0)
endfunction

let s:mySNR = ''
function! s:SNR()
  if s:mySNR == ''
    let s:mySNR = matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSNR$')
  endif
  return s:mySNR
endfun


"" --- START save/restore position. {{{

function! genutils#SaveSoftPosition(id)
  let b:sp_startline_{a:id} = getline(".")
  call genutils#SaveHardPosition(a:id)
endfunction

function! genutils#RestoreSoftPosition(id)
  0
  call genutils#RestoreHardPosition(a:id)
  let stLine = b:sp_startline_{a:id}
  if getline('.') !=# stLine
    if ! search('\V\^'.escape(stLine, "\\").'\$', 'W') 
      call search('\V\^'.escape(stLine, "\\").'\$', 'bW')
    endif
  endif
  call genutils#MoveCurLineToWinLine(b:sp_winline_{a:id})
endfunction

function! genutils#ResetSoftPosition(id)
  unlet b:sp_startline_{a:id}
endfunction

" A synonym for genutils#SaveSoftPosition.
function! genutils#SaveHardPositionWithContext(id)
  call genutils#SaveSoftPosition(a:id)
endfunction

" A synonym for genutils#RestoreSoftPosition.
function! genutils#RestoreHardPositionWithContext(id)
  call genutils#RestoreSoftPosition(a:id)
endfunction

" A synonym for genutils#ResetSoftPosition.
function! genutils#ResetHardPositionWithContext(id)
  call genutils#ResetSoftPosition(a:id)
endfunction

function! genutils#SaveHardPosition(id)
  let b:sp_col_{a:id} = virtcol(".")
  let b:sp_lin_{a:id} = line(".")
  " Avoid accounting for wrapped lines.
  let _wrap = &l:wrap
  try
    setl nowrap
    let b:sp_winline_{a:id} = winline()
  finally
    let &l:wrap = _wrap
  endtry
endfunction

function! genutils#RestoreHardPosition(id)
  " This doesn't take virtual column.
  "call cursor(b:sp_lin_{a:id}, b:sp_col_{a:id})
  " Vim7 generates E16 if line number is invalid.
  " TODO: Why is this leaving cursor on the last-but-one line when the
  " condition meets?
  execute ((line('$') < b:sp_lin_{a:id}) ? line('$') :
        \ b:sp_lin_{a:id})
  "execute b:sp_lin_{a:id}
  execute ((line('$') < b:sp_lin_{a:id}) ? line('$') :
        \ b:sp_lin_{a:id})
  "execute b:sp_lin_{a:id}
  execute "normal!" b:sp_col_{a:id} . "|"
  call genutils#MoveCurLineToWinLine(b:sp_winline_{a:id})
endfunction

function! genutils#ResetHardPosition(id)
  unlet b:sp_col_{a:id}
  unlet b:sp_lin_{a:id}
  unlet b:sp_winline_{a:id}
endfunction

function! genutils#GetLinePosition(id)
  return b:sp_lin_{a:id}
endfunction

function! genutils#GetColPosition(id)
  return b:sp_col_{a:id}
endfunction

function! genutils#IsPositionSet(id)
  return exists('b:sp_col_' . a:id)
endfunction

"" --- END save/restore position. }}}



""
"" --- START: Notify window close -- {{{
""

let s:notifyWindow = {}
function! genutils#AddNotifyWindowClose(windowTitle, functionName)
  let bufName = a:windowTitle

  let s:notifyWindow[bufName] = a:functionName

  "let g:notifyWindow = s:notifyWindow " Debug.

  " Start listening to events.
  aug NotifyWindowClose
    au!
    au WinEnter * :call genutils#CheckWindowClose()
    au BufEnter * :call genutils#CheckWindowClose()
  aug END
endfunction

function! genutils#RemoveNotifyWindowClose(windowTitle)
  let bufName = a:windowTitle

  if has_key(s:notifyWindow, bufName)
    call remove(s:notifyWindow, bufName)
    if len(s:notifyWindow) == 0
      "unlet g:notifyWindow " Debug.
  
      aug NotifyWindowClose
        au!
      aug END
    endif
  endif
endfunction

function! genutils#CheckWindowClose()
  if !exists('s:notifyWindow')
    return
  endif

  " Now iterate over all the registered window titles and see which one's are
  "   closed.
  for nextWin in keys(s:notifyWindow)
    if bufwinnr(s:FindBufferForName(nextWin)) == -1
      let funcName = s:notifyWindow[nextWin]
      " Remove this entry as these are going to be processed. The caller can add
      "   them back if needed.
      unlet s:notifyWindow[nextWin]
      "call input("cmd: " . cmd)
      call call(funcName, [nextWin])
    endif
  endfor
endfunction

"function! NotifyWindowCloseF(title)
"  call input(a:title . " closed")
"endfunction
"
"function! RunNotifyWindowCloseTest()
"  call input("Creating three windows, 'ABC', 'XYZ' and 'b':")
"  split ABC
"  split X Y Z
"  split b
"  redraw
"  call genutils#AddNotifyWindowClose("ABC", "NotifyWindowCloseF")
"  call genutils#AddNotifyWindowClose("X Y Z", "NotifyWindowCloseF")
"  call input("notifyWindow: " . string(s:notifyWindow))
"  au NotifyWindowClose WinEnter
"  call input("Added notifications for 'ABC' and 'XYZ'\n".
"       \ "Now closing the windows, you should see ONLY two notifications:")
"  quit
"  quit
"  quit
"endfunction

""
"" --- END: Notify window close -- }}}
""

" TODO: For large ranges, the cmd can become too big, so make it one cmd per
"       line.
function! genutils#ShowLinesWithSyntax() range
  " This makes sure we start (subsequent) echo's on the first line in the
  " command-line area.
  "
  echo ''

  let cmd        = ''
  let prev_group = ' x '     " Something that won't match any syntax group.

  let show_line = a:firstline
  let isMultiLine = ((a:lastline - a:firstline) > 1)
  while show_line <= a:lastline
    let cmd = ''
    let length = strlen(getline(show_line))
    let column = 1

    while column <= length
      let group = synIDattr(synID(show_line, column, 1), 'name')
      if group != prev_group
        if cmd != ''
          let cmd = cmd . "'|"
        endif
        let cmd = cmd . 'echohl ' . (group == '' ? 'NONE' : group) . "|echon '"
        let prev_group = group
      endif
      let char = strpart(getline(show_line), column - 1, 1)
      if char == "'"
        let char = "'."'".'"
      endif
      let cmd = cmd . char
      let column = column + 1
    endwhile

    try
      exec cmd."\n'"
    catch
      echo ''
    endtry
    let show_line = show_line + 1
  endwhile
  echohl NONE
endfunction


function! genutils#AlignWordWithWordInPreviousLine()
  "First go to the first col in the word.
  if getline('.')[col('.') - 1] =~ '\s'
    normal! w
  else
    normal! "_yiw
  endif
  let orgVcol = virtcol('.')
  let prevLnum = prevnonblank(line('.') - 1)
  if prevLnum == -1
    return
  endif
  let prevLine = getline(prevLnum)

  " First get the column to align with.
  if prevLine[orgVcol - 1] =~ '\s'
    " column starts from 1 where as index starts from 0.
    let nonSpaceStrInd = orgVcol " column starts from 1 where as index starts from 0.
    while prevLine[nonSpaceStrInd] =~ '\s'
      let nonSpaceStrInd = nonSpaceStrInd + 1
    endwhile
  else
    if strlen(prevLine) < orgVcol
      let nonSpaceStrInd = strlen(prevLine) - 1
    else
      let nonSpaceStrInd = orgVcol - 1
    endif

    while prevLine[nonSpaceStrInd - 1] !~ '\s' && nonSpaceStrInd > 0
      let nonSpaceStrInd = nonSpaceStrInd - 1
    endwhile
  endif
  let newVcol = nonSpaceStrInd + 1 " Convert to column number.

  if orgVcol > newVcol " We need to reduce the spacing.
    let sub = strpart(getline('.'), newVcol - 1, (orgVcol - newVcol))
    if sub =~ '^\s\+$'
      " Remove the excess space.
      exec 'normal! ' . newVcol . '|'
      exec 'normal! ' . (orgVcol - newVcol) . 'x'
    endif
  elseif orgVcol < newVcol " We need to insert spacing.
    exec 'normal! ' . orgVcol . '|'
    exec 'normal! ' . (newVcol - orgVcol) . 'i '
  endif
endfunction

function! genutils#ShiftWordInSpace(dir)
  if a:dir == 1 " forward.
    " If currently on <Space>...
    if getline(".")[col(".") - 1] == " "
      let move1 = 'wf '
    else
      " If next col is a 
      "if getline(".")[col(".") + 1]
      let move1 = 'f '
    endif
    let removeCommand = "x"
    let pasteCommand = "bi "
    let move2 = 'w'
    let offset = 0
  else " backward.
    " If currently on <Space>...
    if getline(".")[col(".") - 1] == " "
      let move1 = 'w'
    else
      let move1 = '"_yiW'
    endif
    let removeCommand = "hx"
    let pasteCommand = 'h"_yiwEa '
    let move2 = 'b'
    let offset = -3
  endif

  let savedCol = col(".")
  exec "normal!" move1
  let curCol = col(".")
  let possible = 0
  " Check if there is a space at the end.
  if col("$") == (curCol + 1) " Works only for forward case, as expected.
    let possible = 1
  elseif getline(".")[curCol + offset] == " "
    " Remove the space from here.
    exec "normal!" removeCommand
    let possible = 1
  endif

  " Move back into the word.
  "exec "normal!" savedCol . "|"
  if possible == 1
    exec "normal!" pasteCommand
    exec "normal!" move2
  else
    " Move to the original location.
    exec "normal!" savedCol . "|"
  endif
endfunction


function! genutils#CenterWordInSpace()
  let line = getline('.')
  let orgCol = col('.')
  " If currently on <Space>...
  if line[orgCol - 1] == " "
    let matchExpr = ' *\%'. orgCol . 'c *\w\+ \+'
  else
    let matchExpr = ' \+\(\w*\%' . orgCol . 'c\w*\) \+'
  endif
  let matchInd = match(line, matchExpr)
  if matchInd == -1
    return
  endif
  let matchStr = matchstr(line,  matchExpr)
  let nSpaces = strlen(substitute(matchStr, '[^ ]', '', 'g'))
  let word = substitute(matchStr, ' ', '', 'g')
  let middle = nSpaces / 2
  let left = nSpaces - middle
  let newStr = ''
  while middle > 0
    let newStr = newStr . ' '
    let middle = middle - 1
  endwhile
  let newStr = newStr . word
  while left > 0
    let newStr = newStr . ' '
    let left = left - 1
  endwhile

  let newLine = strpart(line, 0, matchInd)
  let newLine = newLine . newStr
  let newLine = newLine . strpart (line, matchInd + strlen(matchStr))
  silent! keepjumps call setline(line('.'), newLine)
endfunction

function! genutils#MapAppendCascaded(lhs, rhs, mapMode)

  " Determine the map mode from the map command.
  let mapChar = strpart(a:mapMode, 0, 1)

  " Check if this is already mapped.
  let oldrhs = maparg(a:lhs, mapChar)
  if oldrhs != ""
    let self = oldrhs
  else
    let self = a:lhs
  endif
  "echomsg a:mapMode . "oremap" . " " . a:lhs . " " . self . a:rhs
  exec a:mapMode . "oremap" a:lhs self . a:rhs
endfunction

function! genutils#UserFileComplete(ArgLead, CmdLine, CursorPos, smartSlash,
      \ searchPath)
  return genutils#UserFileComplete2(a:ArgLead, a:CmdLine, a:CursorPos,
        \ {'resultsAsList': 0, 'relativePaths': 1, 'smartSlash': a:smartSlash,
        \  'searchPath': a:searchPath})
endfunction

function! genutils#UserDirComplete2(ArgLead, CmdLine, CursorPos, ...)
  let params = a:0 ? a:1 : {}
  return genutils#UserFileComplete2(a:ArgLead, a:CmdLine, a:CursorPos,
        \ extend(params, {'completionTypes': ['dir']}))
endfunction

function! genutils#UserFileComplete2(ArgLead, CmdLine, CursorPos, ...)
  let params = a:0 ? a:1 : {}
  let smartSlash = get(params, 'smartSlash', 1)
  let searchPath = get(params, 'searchPath', '')
  let relativePaths = get(params, 'relativePaths', 0)
  let completionTypes = get(params, 'completionTypes', ['file', 'dir'])
  let anchorAtStart = get(params, 'anchorAtStart', 1)
  let resultsAsList = get(params, 'resultsAsList', 1)
  let includeOriginal = get(params, 'includeOriginal', 1)
  let dedupe = get(params, 'dedupe', 0)
  let opathsep = "\\"
  let npathsep = '/'
  if exists('+shellslash') && ! &shellslash && smartSlash &&
        \ stridx(a:ArgLead, '/') == -1
    let opathsep = '/'
    let npathsep = "\\"
  endif
  let matchMap = {}
  let allMatches = []
  let includeDirs = index(completionTypes, 'dir') != -1
  let includeFiles = index(completionTypes, 'file') != -1
  let _shellslash = &shellslash
  let ArgHead = fnamemodify(a:ArgLead, ':h')
  " Also remove any trailing slashes, for consistency.
  let ArgHead = ArgHead == '.' ? '' :
        \ (genutils#PathIsAbsolute(ArgHead) || ArgHead =~ '^\~' ?
        \  substitute(genutils#CleanupFileName(ArgHead), '\(.\)[\\/]$', '\1', '') :
        \  ArgHead)
  let ArgTail = fnamemodify(a:ArgLead, ':t')
  let pat = (ArgHead == '' && ArgTail == '') ? '*' :
        \    (ArgTail == '' ? ArgHead.'/' :
        \     (ArgHead == '' ? '' : ArgHead.'/').(anchorAtStart ? '' : '*')
        \     .ArgTail).'*'
  for nextPath in split(searchPath, genutils#CrUnProtectedCharsPattern(','), 1)
    " Ignore paths if the ArgHead happens to be an absolute path.
    let nextPath = genutils#PathIsAbsolute(ArgHead) ? '' : 
          \ genutils#CleanupFileName(nextPath).npathsep
    let matches = split(glob(nextPath.pat), "\n")
    if len(matches) != 0
      call map(matches, 'substitute(v:val, opathsep, npathsep, "g").(isdirectory(v:val) ? npathsep : "")')
      call filter(matches, 'v:val[-1:] == npathsep ? includeDirs : includeFiles')
      if relativePaths
        let pathRE = '\V'.escape(substitute(nextPath, opathsep, npathsep, 'g'), "\\")
        call map(matches, 'substitute(v:val, pathRE, "", "g")')
      endif
      if dedupe
        call filter(matches, '!has_key(matchMap, v:val)')
        for match in matches
          let matchMap[match] = 1
        endfor
      endif
      let allMatches += matches
    endif
  endfor
  if includeOriginal
    let allMatches += [a:ArgLead]
  endif
  return resultsAsList ? allMatches : join(allMatches, "\n")
endfunction

command! -complete=file -nargs=* GUDebugEcho :echo <q-args>
function! genutils#UserFileExpand(fileArgs)
  return substitute(genutils#GetVimCmdOutput(
        \ 'GUDebugEcho ' . a:fileArgs), '^\_s\+\|\_s\+$', '', 'g')
endfunction

function! genutils#GetVimCmdOutput(cmd)
  let v:errmsg = ''
  let output = ''
  let _shortmess = &shortmess
  try
    set shortmess=
    redir => output
    silent exec a:cmd
  catch /.*/
    let v:errmsg = substitute(v:exception, '^[^:]\+:', '', '')
  finally
    redir END
    let &shortmess = _shortmess
    if v:errmsg != ''
      let output = ''
    endif
  endtry
  return output
endfunction

function! genutils#OptClearBuffer()
  " Go as far as possible in the undo history to conserve Vim resources.
  let _modifiable = &l:modifiable
  let _undolevels = &undolevels
  try
    setl modifiable
    set undolevels=-1
    silent! keepjumps 0,$delete _
  finally
    let &undolevels = _undolevels
    let &l:modifiable = _modifiable
  endtry
endfunction


"" START: Sorting support. {{{
""

"
" Comapare functions.
"

function! genutils#CmpByLineLengthNname(line1, line2, ...)
  let direction = (a:0?a:1:1)
  let cmp = genutils#CmpByLength(a:line1, a:line2, direction)
  if cmp == 0
    let cmp = genutils#CmpByString(a:line1, a:line2, direction)
  endif
  return cmp
endfunction

function! genutils#CmpByLength(line1, line2, ...)
  let direction = (a:0?a:1:1)
  let len1 = strlen(a:line1)
  let len2 = strlen(a:line2)
  return direction * (len1 - len2)
endfunction

" Compare first by name and then by length.
" Useful for sorting Java imports.
function! genutils#CmpJavaImports(line1, line2, ...)
  let direction = (a:0?a:1:1)
  " FIXME: Simplify this.
  if stridx(a:line1, '.') == -1
    let pkg1 = ''
    let cls1 = substitute(a:line1, '.* \(^[ ]\+\)', '\1', '')
  else
    let pkg1 = substitute(a:line1, '.*import\s\+\(.*\)\.[^. ;]\+.*$', '\1', '')
    let cls1 = substitute(a:line1, '^.*\.\([^. ;]\+\).*$', '\1', '')
  endif
  if stridx(a:line2, '.') == -1
    let pkg2 = ''
    let cls2 = substitute(a:line2, '.* \(^[ ]\+\)', '\1', '')
  else
    let pkg2 = substitute(a:line2, '.*import\s\+\(.*\)\.[^. ;]\+.*$', '\1', '')
    let cls2 = substitute(a:line2, '^.*\.\([^. ;]\+\).*$', '\1', '')
  endif

  let cmp = genutils#CmpByString(pkg1, pkg2, direction)
  if cmp == 0
    let cmp = genutils#CmpByLength(cls1, cls2, direction)
  endif
  return cmp
endfunction

function! genutils#CmpByString(line1, line2, ...)
  let direction = (a:0?a:1:1)
  if a:line1 < a:line2
    return -direction
  elseif a:line1 > a:line2
    return direction
  else
    return 0
  endif
endfunction

function! genutils#CmpByStringIgnoreCase(line1, line2, ...)
  let direction = (a:0?a:1:1)
  if a:line1 <? a:line2
    return -direction
  elseif a:line1 >? a:line2
    return direction
  else
    return 0
  endif
endfunction

function! genutils#CmpByNumber(line1, line2, ...)
  let direction = (a:0 ? a:1 :1)
  let num1 = a:line1 + 0
  let num2 = a:line2 + 0

  if num1 < num2
    return -direction
  elseif num1 > num2
    return direction
  else
    return 0
  endif
endfunction

function! genutils#QSort(cmp, direction) range
  call s:QSortR(a:firstline, a:lastline, a:cmp, a:direction,
        \ 's:BufLineAccessor', 's:BufLineSwapper', '')
endfunction

function! genutils#QSort2(start, end, cmp, direction, accessor, swapper, context)
  call s:QSortR(a:start, a:end, a:cmp, a:direction, a:accessor, a:swapper,
        \ a:context)
endfunction

" The default swapper that swaps lines in the current buffer.
function! s:BufLineSwapper(line1, line2, context)
  let str2 = getline(a:line1)
  silent! keepjumps call setline(a:line1, getline(a:line2))
  silent! keepjumps call setline(a:line2, str2)
endfunction

" The default accessor that returns lines from the current buffer.
function! s:BufLineAccessor(line, context)
  return getline(a:line)
endfunction

" The default mover that moves lines from one place to another in the current
" buffer.
function! s:BufLineMover(from, to, context)
  let line = getline(a:from)
  exec a:from.'d_'
  call append(a:to, line)
endfunction

"
" Sort lines.  QSortR() is called recursively.
"
function! s:QSortR(start, end, cmp, direction, accessor, swapper, context)
  if a:end > a:start
    let low = a:start
    let high = a:end

    " Arbitrarily establish partition element at the midpoint of the data.
    let midStr = {a:accessor}(((a:start + a:end) / 2), a:context)

    " Loop through the data until indices cross.
    while low <= high

      " Find the first element that is greater than or equal to the partition
      "   element starting from the left Index.
      while low < a:end
        let result = {a:cmp}({a:accessor}(low, a:context), midStr, a:direction)
        if result < 0
          let low = low + 1
        else
          break
        endif
      endwhile

      " Find an element that is smaller than or equal to the partition element
      "   starting from the right Index.
      while high > a:start
        let result = {a:cmp}({a:accessor}(high, a:context), midStr, a:direction)
        if result > 0
          let high = high - 1
        else
          break
        endif
      endwhile

      " If the indexes have not crossed, swap.
      if low <= high
        " Swap lines low and high.
        call {a:swapper}(high, low, a:context)
        let low = low + 1
        let high = high - 1
      endif
    endwhile

    " If the right index has not reached the left side of data must now sort
    "   the left partition.
    if a:start < high
      call s:QSortR(a:start, high, a:cmp, a:direction, a:accessor, a:swapper,
            \ a:context)
    endif

    " If the left index has not reached the right side of data must now sort
    "   the right partition.
    if low < a:end
      call s:QSortR(low, a:end, a:cmp, a:direction, a:accessor, a:swapper,
            \ a:context)
    endif
  endif
endfunction

function! genutils#BinSearchForInsert(start, end, line, cmp, direction)
  return genutils#BinSearchForInsert2(a:start, a:end, a:line, a:cmp,
        \ a:direction, 's:BufLineAccessor', '')
endfunction

function! genutils#BinSearchForInsert2(start, end, line, cmp, direction,
      \ accessor, context)
  let start = a:start - 1
  let end = a:end
  while start < end
    let middle = (start + end + 1) / 2
    " Support passing both Funcref's as well as names.
    if type(a:cmp) == 2
      if type(a:accessor) == 2
        let result = a:cmp(a:accessor(middle, a:context), a:line, a:direction)
      else
        let result = a:cmp({a:accessor}(middle, a:context), a:line, a:direction)
      endif
    else
      if type(a:accessor) == 2
        let result = {a:cmp}(a:accessor(middle, a:context), a:line, a:direction)
      else
        let result = {a:cmp}({a:accessor}(middle, a:context), a:line, a:direction)
      endif
    endif
    if result < 0
      let start = middle
    else
      let end = middle - 1
    endif
  endwhile
  return start
endfunction

function! genutils#BinSearchList(list, start, end, item, cmp)
  let start = a:start - 1
  let end = a:end
  while start < end
    let middle = (start + end + 1) / 2
    let result = call(a:cmp, [get(a:list, middle), a:item])
    if result < 0
      let start = middle
    else
      let end = middle - 1
    endif
  endwhile
  return start
endfunction

function! genutils#BinInsertSort(cmp, direction) range
  call genutils#BinInsertSort2(a:firstline, a:lastline, a:cmp, a:direction,
        \ 's:BufLineAccessor', 's:BufLineMover', '')
endfunction

function! genutils#BinInsertSort2(start, end, cmp, direction, accessor, mover, context)
  let i = a:start + 1
  while i <= a:end
    let low = s:BinSearchToAppend2(a:start, i, {a:accessor}(i, a:context),
          \ a:cmp, a:direction, a:accessor, a:context)
    " Move the object.
    if low < i
      call {a:mover}(i, low - 1, a:context)
    endif
    let i = i + 1
  endwhile
endfunction

function! s:BinSearchToAppend(start, end, line, cmp, direction)
  return s:BinSearchToAppend2(a:start, a:end, a:line, a:cmp, a:direction,
        \ 's:BufLineAccessor', '')
endfunction

function! s:BinSearchToAppend2(start, end, line, cmp, direction, accessor,
      \ context)
  let low = a:start
  let high = a:end
  while low < high
    let mid = (low + high) / 2
    let diff = {a:cmp}({a:accessor}(mid, a:context), a:line, a:direction)
    if diff > 0
      let high = mid
    else
      let low = mid + 1
      if diff == 0
        break
      endif
    endif
  endwhile
  return low
endfunction

""" END: Sorting support. }}}


" Eats character if it matches the given pattern.
"
" Originally,
" From: Benji Fisher <fisherbb@bc.edu>
" Date: Mon, 25 Mar 2002 15:05:14 -0500
"
" Based on Bram's idea of eating a character while type <Space> to expand an
"   abbreviation. This solves the problem with abbreviations, where we are
"   left with an extra space after the expansion.
" Ex:
"   inoreabbr \stdout\ System.out.println("");<Left><Left><Left><C-R>=genutils#EatChar('\s')<CR>
function! genutils#EatChar(pat)
   let c = nr2char(getchar())
   "call input('Pattern: '.a:pat.' '.
   "      \ ((c =~ a:pat) ? 'Returning empty' : 'Returning: '.char2nr(c)))
   return (c =~ a:pat) ? '' : c
endfun


" Can return a spacer from 0 to 80 characters width.
let s:spacer= "                                                               ".
      \ "                 "
function! genutils#GetSpacer(width)
  return strpart(s:spacer, 0, a:width)
endfunction

function! genutils#SilentSubstitute(pat, cmd)
  call genutils#SaveHardPosition('SilentSubstitute')
  let _search = @/
  try
    let @/ = a:pat
    keepjumps silent! exec a:cmd
  finally
    let @/ = _search
    call genutils#RestoreHardPosition('SilentSubstitute')
    call genutils#ResetHardPosition('SilentSubstitute')
  endtry
endfunction

function! genutils#SilentDelete(arg1, ...)
  " For backwards compatibility.
  if a:0
    let range = a:arg1
    let pat = a:1
  else
    let range = ''
    let pat = a:arg1
  endif
  let _search = @/
  call genutils#SaveHardPosition('SilentDelete')
  try
    let @/ = pat
    keepjumps silent! exec range'g//d _'
  finally
    let @/ = _search
    call genutils#RestoreHardPosition('SilentDelete')
    call genutils#ResetHardPosition('SilentDelete')
  endtry
endfunction

" START: genutils#Roman2Decimal {{{
let s:I = 1
let s:V = 5
let s:X = 10
let s:L = 50
let s:C = 100
let s:D = 500
let s:M = 1000

function! s:Char2Num(c)
  " A bit of magic on empty strings
  if a:c == ""
    return 0
  endif
  exec 'let n = s:' . toupper(a:c)
  return n
endfun

function! genutils#Roman2Decimal(str)
  if a:str !~? '^[IVXLCDM]\+$'
    return a:str
  endif
  let sum = 0
  let i = 0
  let n0 = s:Char2Num(a:str[i])
  let len = strlen(a:str)
  while i < len
    let i = i + 1
    let n1 = s:Char2Num(a:str[i])
    " Magic: n1=0 when i exceeds len
    if n1 > n0
      let sum = sum - n0
    else
      let sum = sum + n0
    endif
    let n0 = n1
  endwhile
  return sum
endfun
" END: genutils#Roman2Decimal }}}


" BEGIN: Relative path {{{
function! genutils#CommonPath(path1, path2, ...)
  let path1 = genutils#CleanupFileName(a:path1) . ((a:0 > 0 ? a:1 : 0) ? '/' : '')
  let path2 = genutils#CleanupFileName(a:path2) . ((a:0 > 1 ? a:2 : 0) ? '/' : '')
  return genutils#CommonString(path1, path2)
endfunction

function! genutils#CommonString(str1, str2)
  let str1 = a:str1
  let str2 = a:str2
  if str1 == str2
    return str1
  endif
  let n = 0
  while str1[n] == str2[n]
    let n = n+1
  endwhile
  return strpart(str1, 0, n)
endfunction

function! genutils#RelPathFromFile(srcFile, tgtFile)
  return genutils#RelPathFromDir(fnamemodify(a:srcFile, ':h'), a:tgtFile)
endfunction

function! genutils#RelPathFromDir(srcDir, tgtFile)
  let cleanDir = genutils#CleanupFileName(a:srcDir).'/'
  let cleanFile = genutils#CleanupFileName(a:tgtFile)
  let cmnPath = genutils#CommonPath(cleanDir, cleanFile, 1)
  let relDirFromCmnPath = strpart(cleanDir, strlen(cmnPath))
  if relDirFromCmnPath == '/' " This means the file is under the srcDir.
    let relDirFromCmnPath = ''
  endif
  let relFileFromCmnPath = strpart(cleanFile, strlen(cmnPath))
  return substitute(relDirFromCmnPath, '[^/]\+', '..', 'g') .
        \ relFileFromCmnPath
endfunction

" END: Relative path }}}


" BEGIN: Persistent settings {{{
if ! exists("g:genutilsNoPersist") || ! g:genutilsNoPersist
  " Make sure the '!' option to store global variables that are upper cased are
  "   stored in viminfo file.
  " Make sure it is the first option, so that it will not interfere with the
  "   'n' option ("Todd J. Cosgrove" (todd dot cosgrove at softechnics dot
  "   com)).
  " Also take care of empty value, when 'compatible' mode is on (Bram
  "   Moolenar)
  if &viminfo == ''
    set viminfo=!,'20,"50,h
  else
    set viminfo^=!
  endif
endif

function! genutils#PutPersistentVar(pluginName, persistentVar, value)
  if ! exists("g:genutilsNoPersist") || ! g:genutilsNoPersist
    let globalVarName = s:PersistentVarName(a:pluginName, a:persistentVar)
    exec 'let ' . globalVarName . " = '" . a:value . "'"
  endif
endfunction

function! genutils#GetPersistentVar(pluginName, persistentVar, default)
  if ! exists("g:genutilsNoPersist") || ! g:genutilsNoPersist
    let globalVarName = s:PersistentVarName(a:pluginName, a:persistentVar)
    if (exists(globalVarName))
      exec 'let value = ' . globalVarName
      exec 'unlet ' . globalVarName
    else
      let value = a:default
    endif
    return value
  else
    return default
  endif
endfunction

function! s:PersistentVarName(pluginName, persistentVar)
  return 'g:GU_' . toupper(a:pluginName) . '_' . toupper(a:persistentVar)
endfunction
" END: Persistent settings }}}


" FileChangedShell handling {{{
if !exists('s:fcShellPreFuncs')
  let s:fcShellPreFuncs = {}
endif

function! genutils#AddToFCShellPre(funcName)
  let s:fcShellPreFuncs[a:funcName] = a:funcName
endfunction

function! genutils#RemoveFromFCShellPre(funcName)
  if has_key(s:fcShellPreFuncs, a:funcName)
    unlet s:fcShellPreFuncs[a:funcName]
  endif
endfunction

let s:defFCShellInstalled = 0
function! genutils#DefFCShellInstall()
  if ! s:defFCShellInstalled
    aug DefFCShell
    au!
    au FileChangedShell * nested call genutils#DefFileChangedShell()
    aug END
  endif
  let s:defFCShellInstalled = s:defFCShellInstalled + 1
endfunction

function! genutils#DefFCShellUninstall()
  if s:defFCShellInstalled <= 0
    return
  endif
  let s:defFCShellInstalled = s:defFCShellInstalled - 1
  if ! s:defFCShellInstalled
    aug DefFCShell
    au!
    aug END
  endif
endfunction

function! genutils#DefFileChangedShell()
  let autoread = s:InvokeFuncs(s:fcShellPreFuncs)
  let bufNo = expand('<abuf>') + 0
  let fName = expand('<afile>')
  let msg = ''
  let v:fcs_choice = ''
  if getbufvar(bufNo, '&modified')
    let v:fcs_choice = 'ask'
  elseif ! autoread
    let v:fcs_choice = 'ask'
  else
    let v:fcs_choice = 'reload'
  endif
  return
endfunction

function! s:InvokeFuncs(funcList)
  let autoread = &autoread
  if len(a:funcList) != 0
    for nextFunc in keys(a:funcList)
      let result = call(nextFunc, [])
      if result != -1
        let autoread = autoread || result
      endif
    endfor
  endif
  return autoread
endfunction
" FileChangedShell handling }}}


" Sign related utilities {{{
function! genutils#CurLineHasSign()
  let signs = genutils#GetVimCmdOutput('sign place buffer=' . bufnr('%'), 1)
  return (match(signs,
        \ 'line=' . line('.') . '\s\+id=\d\+\s\+name=VimBreakPt') != -1)
endfunction

function! genutils#ClearAllSigns()
  let signs = genutils#GetVimCmdOutput('sign place buffer=' . bufnr('%'), 1)
  let curIdx = 0
  let pat = 'line=\d\+\s\+id=\zs\d\+\ze\s\+name=VimBreakPt'
  let id = 0
  while curIdx != -1 && curIdx < strlen(signs)
    let id = matchstr(signs, pat, curIdx)
    if id != ''
      exec 'sign unplace ' . id . ' buffer=' . bufnr('%')
    endif
    let curIdx = matchend(signs, pat, curIdx)
  endwhile
endfunction
" }}}

let s:UNPROTECTED_CHAR_PRFX = '\%(\%([^\\]\|^\)\\\%(\\\\\)*\)\@<!' " Doesn't eat slashes.
"let s:UNPROTECTED_CHAR_PRFX = '\\\@<!\%(\\\\\)*' " Eats slashes.
function! genutils#CrUnProtectedCharsPattern(chars, ...)
  let capture = (a:0 > 0?1:0)
  let regex = s:UNPROTECTED_CHAR_PRFX
  let chars = a:chars
  if strlen(chars) > 1
    let chars = '['.chars.']'
  endif
  if capture
    let chars = '\('.chars.'\)'
  endif
  return regex.chars
endfunction

function! genutils#PromptForElement(array, default, msg, skip, useDialog,
      \ nCols)
  let nCols = a:nCols
  let index = 0
  let line = ""
  let element = ""
  let optionsMsg = ""
  let colWidth = &columns / nCols - 1 " Leave a margin of one column as a gap.
  let curCol = 1
  let nElements = len(a:array)
  let newArray = [] " Without the skip element.
  if index(a:array, a:skip) != -1
    let nElements = nElements - 1
  endif
  for element in a:array
    if element ==# a:skip
      continue
    endif
    call add(newArray, element)
    let element = strpart(index."   ", 0, 4) . element
    let eleColWidth = (strlen(element) - 1) / colWidth + 1
    " Fill up the spacer for the rest of the partial column.
    let element = element . genutils#GetSpacer(
          \ eleColWidth * (colWidth + 1) - strlen(element) - 1)
    let wouldBeLength = strlen(line) + strlen(element) + 1
    if wouldBeLength > (curCol * (colWidth + eleColWidth)) &&
          \ wouldBeLength > &columns
      let splitLine = 2 " Split before adding the new element.
    elseif curCol == nCols
      let splitLine = 1 " Split after adding the new element.
    else
      let splitLine = 0
    endif
    if splitLine == 2
      if strlen(line) == &columns
        " Remove the last space as it otherwise results in an extra empty line
        " on the screen.
        let line = strpart(line, 0, strlen(line) - 1)
      endif
      let optionsMsg = optionsMsg . line . "\n"
      let line = element . ' '
      let curCol = strlen(element) / (colWidth + 1)
    else
      let line = line . element . ' '
      if splitLine == 1
        if strlen(line) == &columns
          " Remove the last space as it otherwise results in an extra empty line
          " on the screen.
          let line = strpart(line, 0, strlen(line) - 1)
        endif
        let curCol = 0 " Reset col count.
        let optionsMsg = optionsMsg . line . "\n"
        let line = ""
      endif
    endif
    let curCol = curCol + 1
    let index = index + 1
  endfor
  " Finally if there is anything left in line, then append that too.
  if line.'' != ''
    let optionsMsg = optionsMsg . line . "\n"
    let line = ""
  endif

  " Default index or empty string.
  let default = ''
  if type(a:default) == 0
    let default = a:default
  elseif a:default.'' != ''
    let default = index(a:array, a:default)
  endif
  if a:default == -1
    let default = ''
  endif

  while !exists("selectedElement")
    if a:useDialog
      let s:selection = inputdialog(optionsMsg . a:msg, default)
    else
      let s:selection = input(optionsMsg . a:msg, default)
    endif
    if s:selection.'' == ''
      let selectedElement = ''
      let s:selection = -1
    else
      let s:selection = (s:selection !~# '^\d\+$') ? -1 : (s:selection + 0)
      if s:selection >= 0 && s:selection < nElements
        let selectedElement = newArray[s:selection]
      else
        echohl ERROR | echo "\nInvalid selection, please try again" |
              \ echohl NONE
      endif
    endif
    echo "\n"
  endwhile
  return selectedElement
endfunction

let s:selection = -1
function! genutils#GetSelectedIndex()
  return s:selection
endfunction

" Always match() with 'ignorecase' and 'smartcase' off.
function! s:Match(expr, pat, start)
  let _ic = &ignorecase
  let _scs = &smartcase
  let result = -1
  try
    set noignorecase
    set nosmartcase
    let result = match(a:expr, a:pat, a:start)
  finally
    let &ignorecase = _ic
    let &smartcase = _scs
  endtry
  return result
endfunction
 
" Restore cpo.
let &cpo = s:save_cpo
unlet s:save_cpo

" vim6:fdm=marker et
