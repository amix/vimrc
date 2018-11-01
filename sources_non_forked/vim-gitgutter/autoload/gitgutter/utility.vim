function! gitgutter#utility#supports_overscore_sign()
  if gitgutter#utility#windows()
    return &encoding ==? 'utf-8'
  else
    return &termencoding ==? &encoding || &termencoding == ''
  endif
endfunction

function! gitgutter#utility#setbufvar(buffer, varname, val)
  let buffer = +a:buffer
  let dict = get(getbufvar(buffer, '', {}), 'gitgutter', {})
  let needs_setting = empty(dict)
  let dict[a:varname] = a:val
  if needs_setting
    call setbufvar(buffer, 'gitgutter', dict)
  endif
endfunction

function! gitgutter#utility#getbufvar(buffer, varname, ...)
  let dict = get(getbufvar(a:buffer, ''), 'gitgutter', {})
  if has_key(dict, a:varname)
    return dict[a:varname]
  else
    if a:0
      return a:1
    endif
  endif
endfunction

function! gitgutter#utility#warn(message) abort
  echohl WarningMsg
  echo 'vim-gitgutter: ' . a:message
  echohl None
  let v:warningmsg = a:message
endfunction

function! gitgutter#utility#warn_once(bufnr, message, key) abort
  if empty(gitgutter#utility#getbufvar(a:bufnr, a:key))
    call gitgutter#utility#setbufvar(a:bufnr, a:key, '1')
    echohl WarningMsg
    redraw | echom 'vim-gitgutter: ' . a:message
    echohl None
    let v:warningmsg = a:message
  endif
endfunction

" Returns truthy when the buffer's file should be processed; and falsey when it shouldn't.
" This function does not and should not make any system calls.
function! gitgutter#utility#is_active(bufnr) abort
  return g:gitgutter_enabled &&
        \ !pumvisible() &&
        \ s:is_file_buffer(a:bufnr) &&
        \ s:exists_file(a:bufnr) &&
        \ s:not_git_dir(a:bufnr)
endfunction

function! s:not_git_dir(bufnr) abort
  return s:dir(a:bufnr) !~ '[/\\]\.git\($\|[/\\]\)'
endfunction

function! s:is_file_buffer(bufnr) abort
  return empty(getbufvar(a:bufnr, '&buftype'))
endfunction

" From tpope/vim-fugitive
function! s:winshell()
  return &shell =~? 'cmd' || exists('+shellslash') && !&shellslash
endfunction

" From tpope/vim-fugitive
function! gitgutter#utility#shellescape(arg) abort
  if a:arg =~ '^[A-Za-z0-9_/.-]\+$'
    return a:arg
  elseif s:winshell()
    return '"' . substitute(substitute(a:arg, '"', '""', 'g'), '%', '"%"', 'g') . '"'
  else
    return shellescape(a:arg)
  endif
endfunction

function! gitgutter#utility#file(bufnr)
  return s:abs_path(a:bufnr, 1)
endfunction

" Not shellescaped
function! gitgutter#utility#extension(bufnr) abort
  return fnamemodify(s:abs_path(a:bufnr, 0), ':e')
endfunction

function! gitgutter#utility#system(cmd, ...) abort
  call gitgutter#debug#log(a:cmd, a:000)

  call s:use_known_shell()
  silent let output = (a:0 == 0) ? system(a:cmd) : system(a:cmd, a:1)
  call s:restore_shell()

  return output
endfunction

" Path of file relative to repo root.
"
" *     empty string - not set
" * non-empty string - path
" *               -1 - pending
" *               -2 - not tracked by git
function! gitgutter#utility#repo_path(bufnr, shellesc) abort
  let p = gitgutter#utility#getbufvar(a:bufnr, 'path')
  return a:shellesc ? gitgutter#utility#shellescape(p) : p
endfunction

function! gitgutter#utility#set_repo_path(bufnr) abort
  " Values of path:
  " * non-empty string - path
  " *               -1 - pending
  " *               -2 - not tracked by git

  call gitgutter#utility#setbufvar(a:bufnr, 'path', -1)
  let cmd = gitgutter#utility#cd_cmd(a:bufnr, g:gitgutter_git_executable.' ls-files --error-unmatch --full-name -- '.gitgutter#utility#shellescape(s:filename(a:bufnr)))

  if g:gitgutter_async && gitgutter#async#available()
    if has('lambda')
      call gitgutter#async#execute(cmd, a:bufnr, {
            \   'out': {bufnr, path -> gitgutter#utility#setbufvar(bufnr, 'path', s:strip_trailing_new_line(path))},
            \   'err': {bufnr       -> gitgutter#utility#setbufvar(bufnr, 'path', -2)},
            \ })
    else
      if has('nvim') && !has('nvim-0.2.0')
        call gitgutter#async#execute(cmd, a:bufnr, {
              \   'out': function('s:set_path'),
              \   'err': function('s:not_tracked_by_git')
              \ })
      else
        call gitgutter#async#execute(cmd, a:bufnr, {
              \   'out': function('s:set_path'),
              \   'err': function('s:set_path', [-2])
              \ })
      endif
    endif
  else
    let path = gitgutter#utility#system(cmd)
    if v:shell_error
      call gitgutter#utility#setbufvar(a:bufnr, 'path', -2)
    else
      call gitgutter#utility#setbufvar(a:bufnr, 'path', s:strip_trailing_new_line(path))
    endif
  endif
endfunction

if has('nvim') && !has('nvim-0.2.0')
  function! s:not_tracked_by_git(bufnr)
    call s:set_path(a:bufnr, -2)
  endfunction
endif

function! s:set_path(bufnr, path)
  if a:bufnr == -2
    let [bufnr, path] = [a:path, a:bufnr]
    call gitgutter#utility#setbufvar(bufnr, 'path', path)
  else
    call gitgutter#utility#setbufvar(a:bufnr, 'path', s:strip_trailing_new_line(a:path))
  endif
endfunction

function! gitgutter#utility#cd_cmd(bufnr, cmd) abort
  let cd = s:unc_path(a:bufnr) ? 'pushd' : (gitgutter#utility#windows() ? 'cd /d' : 'cd')
  return cd.' '.s:dir(a:bufnr).' && '.a:cmd
endfunction

function! s:unc_path(bufnr)
  return s:abs_path(a:bufnr, 0) =~ '^\\\\'
endfunction

function! s:use_known_shell() abort
  if has('unix') && &shell !=# 'sh'
    let [s:shell, s:shellcmdflag, s:shellredir] = [&shell, &shellcmdflag, &shellredir]
    let &shell = 'sh'
    set shellcmdflag=-c shellredir=>%s\ 2>&1
  endif
endfunction

function! s:restore_shell() abort
  if has('unix') && exists('s:shell')
    let [&shell, &shellcmdflag, &shellredir] = [s:shell, s:shellcmdflag, s:shellredir]
  endif
endfunction

function! s:abs_path(bufnr, shellesc)
  let p = resolve(expand('#'.a:bufnr.':p'))
  return a:shellesc ? gitgutter#utility#shellescape(p) : p
endfunction

function! s:dir(bufnr) abort
  return gitgutter#utility#shellescape(fnamemodify(s:abs_path(a:bufnr, 0), ':h'))
endfunction

" Not shellescaped.
function! s:filename(bufnr) abort
  return fnamemodify(s:abs_path(a:bufnr, 0), ':t')
endfunction

function! s:exists_file(bufnr) abort
  return filereadable(s:abs_path(a:bufnr, 0))
endfunction

function! s:strip_trailing_new_line(line) abort
  return substitute(a:line, '\n$', '', '')
endfunction

function! gitgutter#utility#windows()
  return has('win64') || has('win32') || has('win16')
endfunction
