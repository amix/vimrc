" Utilities for file copy/move/mkdir/etc.

let s:save_cpo = &cpo
set cpo&vim

let s:is_unix = has('unix')
let s:is_windows = has('win16') || has('win32') || has('win64') || has('win95')
let s:is_cygwin = has('win32unix')
let s:is_mac = !s:is_windows && !s:is_cygwin
      \ && (has('mac') || has('macunix') || has('gui_macvim') ||
      \   (!isdirectory('/proc') && executable('sw_vers')))
" As of 7.4.122, the system()'s 1st argument is converted internally by Vim.
" Note that Patch 7.4.122 does not convert system()'s 2nd argument and
" return-value. We must convert them manually.
let s:need_trans = v:version < 704 || (v:version == 704 && !has('patch122'))

" Open a file.
function! s:open(filename) "{{{
  let filename = fnamemodify(a:filename, ':p')

  " Detect desktop environment.
  if s:is_windows
    " For URI only.
    if s:need_trans
      let filename = iconv(filename, &encoding, 'char')
    endif
    silent execute '!start rundll32 url.dll,FileProtocolHandler' filename
  elseif s:is_cygwin
    " Cygwin.
    call system(printf('%s %s', 'cygstart',
          \ shellescape(filename)))
  elseif executable('xdg-open')
    " Linux.
    call system(printf('%s %s &', 'xdg-open',
          \ shellescape(filename)))
  elseif exists('$KDE_FULL_SESSION') && $KDE_FULL_SESSION ==# 'true'
    " KDE.
    call system(printf('%s %s &', 'kioclient exec',
          \ shellescape(filename)))
  elseif exists('$GNOME_DESKTOP_SESSION_ID')
    " GNOME.
    call system(printf('%s %s &', 'gnome-open',
          \ shellescape(filename)))
  elseif executable('exo-open')
    " Xfce.
    call system(printf('%s %s &', 'exo-open',
          \ shellescape(filename)))
  elseif s:is_mac && executable('open')
    " Mac OS.
    call system(printf('%s %s &', 'open',
          \ shellescape(filename)))
  else
    " Give up.
    throw 'Not supported.'
  endif
endfunction "}}}


" Move a file.
" Dispatch s:move_exe() or s:move_vim().
" FIXME: Currently s:move_vim() does not support
" moving a directory.
function! s:move(src, dest) "{{{
  if s:_has_move_exe() || isdirectory(a:src)
    return s:move_exe(a:src, a:dest)
  else
    return s:move_vim(a:src, a:dest)
  endif
endfunction "}}}

if s:is_unix
  function! s:_has_move_exe()
    return executable('mv')
  endfunction
elseif s:is_windows
  function! s:_has_move_exe()
    return 1
  endfunction
else
  function! s:_has_move_exe()
    throw 'vital: System.File._has_move_exe(): your platform is not supported'
  endfunction
endif

" Move a file.
" Implemented by external program.
if s:is_unix
  function! s:move_exe(src, dest)
    if !s:_has_move_exe() | return 0 | endif
    let [src, dest] = [a:src, a:dest]
    call system('mv ' . shellescape(src) . ' ' . shellescape(dest))
    return !v:shell_error
  endfunction
elseif s:is_windows
  function! s:move_exe(src, dest)
    if !s:_has_move_exe() | return 0 | endif
    let [src, dest] = [a:src, a:dest]
    " Normalize successive slashes to one slash.
    let src  = substitute(src, '[/\\]\+', '\', 'g')
    let dest = substitute(dest, '[/\\]\+', '\', 'g')
    " src must not have trailing '\'.
    let src  = substitute(src, '\\$', '', 'g')
    " All characters must be encoded to system encoding.
    if s:need_trans
      let src  = iconv(src, &encoding, 'char')
      let dest = iconv(dest, &encoding, 'char')
    endif
    let cmd_exe = (&shell =~? 'cmd\.exe$' ? '' : 'cmd /c ')
    call system(cmd_exe . 'move /y ' . src  . ' ' . dest)
    return !v:shell_error
  endfunction
else
  function! s:move_exe()
    throw 'vital: System.File.move_exe(): your platform is not supported'
  endfunction
endif

" Move a file.
" Implemented by pure Vim script.
function! s:move_vim(src, dest) "{{{
  return !rename(a:src, a:dest)
endfunction "}}}

" Copy a file.
" Dispatch s:copy_exe() or s:copy_vim().
function! s:copy(src, dest) "{{{
  if s:_has_copy_exe()
    return s:copy_exe(a:src, a:dest)
  else
    return s:copy_vim(a:src, a:dest)
  endif
endfunction "}}}

if s:is_unix
  function! s:_has_copy_exe()
    return executable('cp')
  endfunction
elseif s:is_windows
  function! s:_has_copy_exe()
    return 1
  endfunction
else
  function! s:_has_copy_exe()
    throw 'vital: System.File._has_copy_exe(): your platform is not supported'
  endfunction
endif

" Copy a file.
" Implemented by external program.
if s:is_unix
  function! s:copy_exe(src, dest)
    if !s:_has_copy_exe() | return 0 | endif
    let [src, dest] = [a:src, a:dest]
    call system('cp ' . shellescape(src) . ' ' . shellescape(dest))
    return !v:shell_error
  endfunction
elseif s:is_windows
  function! s:copy_exe(src, dest)
    if !s:_has_copy_exe() | return 0 | endif
    let [src, dest] = [a:src, a:dest]
    let src  = substitute(src, '/', '\', 'g')
    let dest = substitute(dest, '/', '\', 'g')
    let cmd_exe = (&shell =~? 'cmd\.exe$' ? '' : 'cmd /c ')
    call system(cmd_exe . 'copy /y ' . src . ' ' . dest)
    return !v:shell_error
  endfunction
else
  function! s:copy_exe()
    throw 'vital: System.File.copy_exe(): your platform is not supported'
  endfunction
endif

" Copy a file.
" Implemented by pure Vim script.
function! s:copy_vim(src, dest) "{{{
  let ret = writefile(readfile(a:src, "b"), a:dest, "b")
  if ret == -1
    return 0
  endif
  return 1
endfunction "}}}

" mkdir() but does not throw an exception.
" Returns true if success.
" Returns false if failure.
function! s:mkdir_nothrow(...) "{{{
  try
    return call('mkdir', a:000)
  catch
    return 0
  endtry
endfunction "}}}


" Delete a file/directory.
if s:is_unix
  function! s:rmdir(path, ...)
    let flags = a:0 ? a:1 : ''
    let cmd = flags =~# 'r' ? 'rm -r' : 'rmdir'
    let cmd .= flags =~# 'f' && cmd ==# 'rm -r' ? ' -f' : ''
    let ret = system(cmd . ' ' . shellescape(a:path))
    if v:shell_error
      let ret = iconv(ret, 'char', &encoding)
      throw substitute(ret, '\n', '', 'g')
    endif
  endfunction

elseif s:is_windows
  function! s:rmdir(path, ...)
    let flags = a:0 ? a:1 : ''
    if &shell =~? "sh$"
      let cmd = flags =~# 'r' ? 'rm -r' : 'rmdir'
      let cmd .= flags =~# 'f' && cmd ==# 'rm -r' ? ' -f' : ''
      let ret = system(cmd . ' ' . shellescape(a:path))
    else
      " 'f' flag does not make sense.
      let cmd = 'rmdir /Q'
      let cmd .= flags =~# 'r' ? ' /S' : ''
      let ret = system(cmd . ' "' . a:path . '"')
    endif
    if v:shell_error
      let ret = iconv(ret, 'char', &encoding)
      throw substitute(ret, '\n', '', 'g')
    endif
  endfunction

else
  function! s:rmdir(...)
    throw 'vital: System.File.rmdir(): your platform is not supported'
  endfunction
endif


let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et ts=2 sts=2 sw=2 tw=0:
