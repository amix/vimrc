let s:file = ''
let s:using_xolox_shell = -1
let s:exit_code = 0

function! gitgutter#utility#setbufvar(buffer, varname, val)
  let dict = get(getbufvar(a:buffer, ''), 'gitgutter', {})
  let dict[a:varname] = a:val
  call setbufvar(a:buffer, 'gitgutter', dict)
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

function! gitgutter#utility#warn_once(message, key) abort
  if empty(gitgutter#utility#getbufvar(s:bufnr, a:key))
    call gitgutter#utility#setbufvar(s:bufnr, a:key, '1')
    echohl WarningMsg
    redraw | echo 'vim-gitgutter: ' . a:message
    echohl None
    let v:warningmsg = a:message
  endif
endfunction

" Returns truthy when the buffer's file should be processed; and falsey when it shouldn't.
" This function does not and should not make any system calls.
function! gitgutter#utility#is_active() abort
  return g:gitgutter_enabled &&
        \ !pumvisible() &&
        \ gitgutter#utility#is_file_buffer() &&
        \ gitgutter#utility#exists_file() &&
        \ gitgutter#utility#not_git_dir()
endfunction

function! gitgutter#utility#not_git_dir() abort
  return gitgutter#utility#full_path_to_directory_of_file() !~ '[/\\]\.git\($\|[/\\]\)'
endfunction

function! gitgutter#utility#is_file_buffer() abort
  return empty(getbufvar(s:bufnr, '&buftype'))
endfunction

" A replacement for the built-in `shellescape(arg)`.
"
" Recent versions of Vim handle shell escaping pretty well.  However older
" versions aren't as good.  This attempts to do the right thing.
"
" See:
" https://github.com/tpope/vim-fugitive/blob/8f0b8edfbd246c0026b7a2388e1d883d579ac7f6/plugin/fugitive.vim#L29-L37
function! gitgutter#utility#shellescape(arg) abort
  if a:arg =~ '^[A-Za-z0-9_/.-]\+$'
    return a:arg
  elseif &shell =~# 'cmd' || gitgutter#utility#using_xolox_shell()
    return '"' . substitute(substitute(a:arg, '"', '""', 'g'), '%', '"%"', 'g') . '"'
  else
    return shellescape(a:arg)
  endif
endfunction

function! gitgutter#utility#set_buffer(bufnr) abort
  let s:bufnr = a:bufnr
  let s:file = resolve(bufname(a:bufnr))
endfunction

function! gitgutter#utility#bufnr()
  return s:bufnr
endfunction

function! gitgutter#utility#file()
  return s:file
endfunction

function! gitgutter#utility#filename() abort
  return fnamemodify(s:file, ':t')
endfunction

function! gitgutter#utility#extension() abort
  return fnamemodify(s:file, ':e')
endfunction

function! gitgutter#utility#full_path_to_directory_of_file() abort
  return fnamemodify(s:file, ':p:h')
endfunction

function! gitgutter#utility#directory_of_file() abort
  return fnamemodify(s:file, ':h')
endfunction

function! gitgutter#utility#exists_file() abort
  return filereadable(s:file)
endfunction

function! gitgutter#utility#has_unsaved_changes() abort
  return getbufvar(s:bufnr, "&mod")
endfunction

function! gitgutter#utility#has_fresh_changes() abort
  return getbufvar(s:bufnr, 'changedtick') != gitgutter#utility#getbufvar(s:bufnr, 'last_tick')
endfunction

function! gitgutter#utility#save_last_seen_change() abort
  call gitgutter#utility#setbufvar(s:bufnr, 'last_tick', getbufvar(s:bufnr, 'changedtick'))
endfunction

function! gitgutter#utility#shell_error() abort
  return gitgutter#utility#using_xolox_shell() ? s:exit_code : v:shell_error
endfunction

function! gitgutter#utility#using_xolox_shell() abort
  if s:using_xolox_shell == -1
    if !g:gitgutter_avoid_cmd_prompt_on_windows
      let s:using_xolox_shell = 0
    " Although xolox/vim-shell works on both windows and unix we only want to use
    " it on windows.
    elseif has('win32') || has('win64') || has('win32unix')
      let s:using_xolox_shell = exists('g:xolox#misc#version') && exists('g:xolox#shell#version')
    else
      let s:using_xolox_shell = 0
    endif
  endif
  return s:using_xolox_shell
endfunction

function! gitgutter#utility#system(cmd, ...) abort
  call gitgutter#debug#log(a:cmd, a:000)

  if gitgutter#utility#using_xolox_shell()
    let options = {'command': a:cmd, 'check': 0}
    if a:0 > 0
      let options['stdin'] = a:1
    endif
    let ret = xolox#misc#os#exec(options)
    let output = join(ret.stdout, "\n")
    let s:exit_code = ret.exit_code
  else
    silent let output = (a:0 == 0) ? system(a:cmd) : system(a:cmd, a:1)
  endif
  return output
endfunction

function! gitgutter#utility#file_relative_to_repo_root() abort
  let file_path_relative_to_repo_root = gitgutter#utility#getbufvar(s:bufnr, 'repo_relative_path')
  if empty(file_path_relative_to_repo_root)
    let dir_path_relative_to_repo_root = gitgutter#utility#system(gitgutter#utility#command_in_directory_of_file(g:gitgutter_git_executable.' rev-parse --show-prefix'))
    let dir_path_relative_to_repo_root = gitgutter#utility#strip_trailing_new_line(dir_path_relative_to_repo_root)
    let file_path_relative_to_repo_root = dir_path_relative_to_repo_root . gitgutter#utility#filename()
    call gitgutter#utility#setbufvar(s:bufnr, 'repo_relative_path', file_path_relative_to_repo_root)
  endif
  return file_path_relative_to_repo_root
endfunction

function! gitgutter#utility#command_in_directory_of_file(cmd) abort
  return 'cd '.gitgutter#utility#shellescape(gitgutter#utility#directory_of_file()).' && '.a:cmd
endfunction

function! gitgutter#utility#highlight_name_for_change(text) abort
  if a:text ==# 'added'
    return 'GitGutterLineAdded'
  elseif a:text ==# 'removed'
    return 'GitGutterLineRemoved'
  elseif a:text ==# 'removed_first_line'
    return 'GitGutterLineRemovedFirstLine'
  elseif a:text ==# 'modified'
    return 'GitGutterLineModified'
  elseif a:text ==# 'modified_removed'
    return 'GitGutterLineModifiedRemoved'
  endif
endfunction

" Dedups list in-place.
" Assumes list has no empty entries.
function! gitgutter#utility#dedup(list)
  return filter(sort(a:list), 'index(a:list, v:val, v:key + 1) == -1')
endfunction

function! gitgutter#utility#strip_trailing_new_line(line) abort
  return substitute(a:line, '\n$', '', '')
endfunction

" True for git v1.7.2+.
function! gitgutter#utility#git_supports_command_line_config_override() abort
  call system(g:gitgutter_git_executable.' -c foo.bar=baz --version')
  return !v:shell_error
endfunction

function! gitgutter#utility#stringify(list) abort
  return join(a:list, "\n")."\n"
endfunction

function! gitgutter#utility#use_known_shell() abort
  if has('unix')
    if &shell !=# 'sh'
      let s:shell = &shell
      let s:shellcmdflag = &shellcmdflag
      let s:shellredir = &shellredir
      let &shell = 'sh'
      set shellcmdflag=-c
      set shellredir=>%s\ 2>&1
    endif
  endif
endfunction

function! gitgutter#utility#restore_shell() abort
  if has('unix')
    if exists('s:shell')
      let &shell = s:shell
      let &shellcmdflag = s:shellcmdflag
      let &shellredir = s:shellredir
    endif
  endif
endfunction
