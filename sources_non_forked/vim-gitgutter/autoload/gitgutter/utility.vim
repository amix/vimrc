function! gitgutter#utility#supports_overscore_sign()
  if gitgutter#utility#windows()
    return &encoding ==? 'utf-8'
  else
    return &termencoding ==? &encoding || &termencoding == ''
  endif
endfunction

function! gitgutter#utility#setbufvar(buffer, varname, val)
  let buffer = +a:buffer
  " Default value for getbufvar() was introduced in Vim 7.3.831.
  let ggvars = getbufvar(buffer, 'gitgutter')
  if type(ggvars) == type('')
    unlet ggvars
    let ggvars = {}
    call setbufvar(buffer, 'gitgutter', ggvars)
  endif
  let ggvars[a:varname] = a:val
endfunction

function! gitgutter#utility#getbufvar(buffer, varname, ...)
  let buffer = +a:buffer
  let ggvars = getbufvar(buffer, 'gitgutter')
  if type(ggvars) == type({}) && has_key(ggvars, a:varname)
    return ggvars[a:varname]
  endif
  if a:0
    return a:1
  endif
endfunction

function! gitgutter#utility#warn(message) abort
  echohl WarningMsg
  echo a:message
  echohl None
  let v:warningmsg = a:message
endfunction

function! gitgutter#utility#warn_once(bufnr, message, key) abort
  if empty(gitgutter#utility#getbufvar(a:bufnr, a:key))
    call gitgutter#utility#setbufvar(a:bufnr, a:key, '1')
    echohl WarningMsg
    redraw | echom a:message
    echohl None
    let v:warningmsg = a:message
  endif
endfunction

" Returns truthy when the buffer's file should be processed; and falsey when it shouldn't.
" This function does not and should not make any system calls.
function! gitgutter#utility#is_active(bufnr) abort
  return gitgutter#utility#getbufvar(a:bufnr, 'enabled') &&
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

function! gitgutter#utility#has_repo_path(bufnr)
  return index(['', -1, -2], gitgutter#utility#repo_path(a:bufnr, 0)) == -1
endfunction

" Path of file relative to repo root.
"
" *     empty string - not set
" * non-empty string - path
" *               -1 - pending
" *               -2 - not tracked by git
" *               -3 - assume unchanged
function! gitgutter#utility#repo_path(bufnr, shellesc) abort
  let p = gitgutter#utility#getbufvar(a:bufnr, 'path', '')
  return a:shellesc ? gitgutter#utility#shellescape(p) : p
endfunction


let s:set_path_handler = {}

function! s:set_path_handler.out(buffer, listing) abort
  let listing = s:strip_trailing_new_line(a:listing)
  let [status, path] = [listing[0], listing[2:]]
  if status =~# '[a-z]'
    call gitgutter#utility#setbufvar(a:buffer, 'path', -3)
  else
    call gitgutter#utility#setbufvar(a:buffer, 'path', path)
  endif

  if type(self.continuation) == type(function('tr'))
    call self.continuation()
  else
    call call(self.continuation.function, self.continuation.arguments)
  endif
endfunction

function! s:set_path_handler.err(buffer) abort
  call gitgutter#utility#setbufvar(a:buffer, 'path', -2)
endfunction


" continuation - a funcref or hash to call after setting the repo path asynchronously.
"
" Returns 'async' if the the path is set asynchronously, 0 otherwise.
function! gitgutter#utility#set_repo_path(bufnr, continuation) abort
  " Values of path:
  " * non-empty string - path
  " *               -1 - pending
  " *               -2 - not tracked by git
  " *               -3 - assume unchanged

  call gitgutter#utility#setbufvar(a:bufnr, 'path', -1)
  let cmd = gitgutter#utility#cd_cmd(a:bufnr,
        \ gitgutter#git().' ls-files -v --error-unmatch --full-name -z -- '.
        \ gitgutter#utility#shellescape(gitgutter#utility#filename(a:bufnr)))

  if g:gitgutter_async && gitgutter#async#available() && !has('vim_starting')
    let handler = copy(s:set_path_handler)
    let handler.continuation = a:continuation
    call gitgutter#async#execute(cmd, a:bufnr, handler)
    return 'async'
  endif

  let listing = gitgutter#utility#system(cmd)

  if v:shell_error
    call gitgutter#utility#setbufvar(a:bufnr, 'path', -2)
    return
  endif

  let listing = s:strip_trailing_new_line(listing)
  let [status, path] = [listing[0], listing[2:]]
  if status =~# '[a-z]'
    call gitgutter#utility#setbufvar(a:bufnr, 'path', -3)
  else
    call gitgutter#utility#setbufvar(a:bufnr, 'path', path)
  endif
endfunction


function! gitgutter#utility#clean_smudge_filter_applies(bufnr)
  let filtered = gitgutter#utility#getbufvar(a:bufnr, 'filter', -1)
  if filtered == -1
    let cmd = gitgutter#utility#cd_cmd(a:bufnr,
          \ gitgutter#git().' check-attr filter -- '.
          \ gitgutter#utility#shellescape(gitgutter#utility#filename(a:bufnr)))
    let out = gitgutter#utility#system(cmd)
    let filtered = out !~ 'unspecified'
    call gitgutter#utility#setbufvar(a:bufnr, 'filter', filtered)
  endif
  return filtered
endfunction


function! gitgutter#utility#cd_cmd(bufnr, cmd) abort
  let cd = s:unc_path(a:bufnr) ? 'pushd' : (gitgutter#utility#windows() && s:dos_shell() ? 'cd /d' : 'cd')
  return cd.' '.s:dir(a:bufnr).' && '.a:cmd
endfunction

function! s:unc_path(bufnr)
  return s:abs_path(a:bufnr, 0) =~ '^\\\\'
endfunction

function! s:dos_shell()
  return &shell == 'cmd.exe' || &shell == 'command.com'
endfunction

function! s:use_known_shell() abort
  if has('unix') && &shell !=# 'sh'
    let [s:shell, s:shellcmdflag, s:shellredir, s:shellpipe, s:shellquote, s:shellxquote] = [&shell, &shellcmdflag, &shellredir, &shellpipe, &shellquote, &shellxquote]
    let &shell = 'sh'
    set shellcmdflag=-c shellredir=>%s\ 2>&1
  endif
  if has('win32') && (&shell =~# 'pwsh' || &shell =~# 'powershell')
    let [s:shell, s:shellcmdflag, s:shellredir, s:shellpipe, s:shellquote, s:shellxquote] = [&shell, &shellcmdflag, &shellredir, &shellpipe, &shellquote, &shellxquote]
    let &shell = 'cmd.exe'
    set shellcmdflag=/s\ /c shellredir=>%s\ 2>&1 shellpipe=>%s\ 2>&1 shellquote= shellxquote="
  endif
endfunction

function! s:restore_shell() abort
  if (has('unix') || has('win32')) && exists('s:shell')
    let [&shell, &shellcmdflag, &shellredir, &shellpipe, &shellquote, &shellxquote] = [s:shell, s:shellcmdflag, s:shellredir, s:shellpipe, s:shellquote, s:shellxquote]
  endif
endfunction

function! gitgutter#utility#get_diff_base(bufnr)
  let p = resolve(expand('#'.a:bufnr.':p'))
  let ml = matchlist(p, '\v^fugitive:/.*/(\x{40,})/')
  if !empty(ml) && !empty(ml[1])
    return ml[1].'^'
  endif
  return g:gitgutter_diff_base
endfunction

function! s:abs_path(bufnr, shellesc)
  let p = resolve(expand('#'.a:bufnr.':p'))

  " Remove extra parts from fugitive's filepaths
  let p = substitute(substitute(p, '^fugitive:', '', ''), '\v\.git/\x{40,}/', '', '')

  return a:shellesc ? gitgutter#utility#shellescape(p) : p
endfunction

function! s:dir(bufnr) abort
  return gitgutter#utility#shellescape(fnamemodify(s:abs_path(a:bufnr, 0), ':h'))
endfunction

" Not shellescaped.
function! gitgutter#utility#filename(bufnr) abort
  return fnamemodify(s:abs_path(a:bufnr, 0), ':t')
endfunction

function! s:exists_file(bufnr) abort
  return filereadable(s:abs_path(a:bufnr, 0))
endfunction

" Get rid of any trailing new line or SOH character.
"
" git ls-files -z produces output with null line termination.
" Vim's system() replaces any null characters in the output
" with SOH (start of header), i.e. ^A.
function! s:strip_trailing_new_line(line) abort
  return substitute(a:line, '[[:cntrl:]]$', '', '')
endfunction

function! gitgutter#utility#windows()
  return has('win64') || has('win32') || has('win16')
endfunction
