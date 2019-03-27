" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

" Spawn starts an asynchronous job. See the description of go#job#Options to
" understand the args parameter.
"
" Spawn returns a job.
function! go#job#Spawn(cmd, args)
  let l:options = go#job#Options(a:args)
  return go#job#Start(a:cmd, l:options)
endfunction

" Options returns callbacks to be used with job_start. It is abstracted to be
" used with various go commands, such as build, test, install, etc.. This
" allows us to avoid writing the same callback over and over for some
" commands. It's fully customizable so each command can change it to its own
" logic.
"
" args is a dictionary with the these keys:
"   'bang':
"     Set to 0 to jump to the first error in the error list.
"     Defaults to 0.
"   'statustype':
"     The status type to use when updating the status.
"     See statusline.vim.
"   'for':
"     The g:go_list_type_command key to use to get the error list type to use.
"     Errors will not be handled when the value is '_'.
"     Defaults to '_job'
"   'errorformat':
"     The errorformat string to use when parsing errors. Defaults to
"     &errorformat.
"     See :help 'errorformat'.
"   'complete':
"     A function to call after the job exits and the channel is closed. The
"     function will be passed three arguments: the job, its exit code, and the
"     list of messages received from the channel. The default is a no-op. A
"     custom value can modify the messages before they are processed by the
"     returned exit_cb and close_cb callbacks. When the function is called,
"     the current window will be the window that was hosting the buffer when
"     the job was started. After it returns, the current window will be
"     restored to what it was before the function was called.

" The return value is a dictionary with these keys:
"   'callback':
"     A function suitable to be passed as a job callback handler. See
"     job-callback.
"   'exit_cb':
"     A function suitable to be passed as a job exit_cb handler. See
"     job-exit_cb.
"   'close_cb':
"     A function suitable to be passed as a job close_cb handler. See
"     job-close_cb.
"   'cwd':
"     The path to the directory which contains the current buffer. The
"     callbacks are configured to expect this directory is the working
"     directory for the job; it should not be modified by callers.
function! go#job#Options(args)
  let cbs = {}
  let state = {
        \ 'winid': win_getid(winnr()),
        \ 'dir': getcwd(),
        \ 'jobdir': fnameescape(expand("%:p:h")),
        \ 'messages': [],
        \ 'bang': 0,
        \ 'for': "_job",
        \ 'exited': 0,
        \ 'exit_status': 0,
        \ 'closed': 0,
        \ 'errorformat': &errorformat,
        \ 'statustype' : ''
      \ }

  if has("patch-8.0.0902") || has('nvim')
    let cbs.cwd = state.jobdir
  endif

  if has_key(a:args, 'bang')
    let state.bang = a:args.bang
  endif

  if has_key(a:args, 'for')
    let state.for = a:args.for
  endif

  if has_key(a:args, 'statustype')
    let state.statustype = a:args.statustype
  endif

  if has_key(a:args, 'errorformat')
    let state.errorformat = a:args.errorformat
  endif

  function state.complete(job, exit_status, data)
    if has_key(self, 'custom_complete')
      let l:winid = win_getid(winnr())
      " Always set the active window to the window that was active when the job
      " was started. Among other things, this makes sure that the correct
      " window's location list will be populated when the list type is
      " 'location' and the user has moved windows since starting the job.
      call win_gotoid(self.winid)
      call self.custom_complete(a:job, a:exit_status, a:data)
      call win_gotoid(l:winid)
    endif

    call self.show_errors(a:job, a:exit_status, a:data)
  endfunction

  function state.show_status(job, exit_status) dict
    if self.statustype == ''
      return
    endif

    if go#config#EchoCommandInfo()
      let prefix = '[' . self.statustype . '] '
      if a:exit_status == 0
        call go#util#EchoSuccess(prefix . "SUCCESS")
      else
        call go#util#EchoError(prefix . "FAIL")
      endif
    endif

    let status = {
          \ 'desc': 'last status',
          \ 'type': self.statustype,
          \ 'state': "success",
          \ }

    if a:exit_status
      let status.state = "failed"
    endif

    if has_key(self, 'started_at')
      let elapsed_time = reltimestr(reltime(self.started_at))
      " strip whitespace
      let elapsed_time = substitute(elapsed_time, '^\s*\(.\{-}\)\s*$', '\1', '')
      let status.state .= printf(" (%ss)", elapsed_time)
    endif

    call go#statusline#Update(self.jobdir, status)
  endfunction

  if has_key(a:args, 'complete')
    let state.custom_complete = a:args.complete
  endif

  " explicitly bind _start to state so that within it, self will
  " always refer to state. See :help Partial for more information.
  "
  " _start is intended only for internal use and should not be referenced
  " outside of this file.
  let cbs._start = function('s:start', [''], state)

  " explicitly bind callback to state so that within it, self will
  " always refer to state. See :help Partial for more information.
  let cbs.callback = function('s:callback', [], state)

  " explicitly bind exit_cb to state so that within it, self will always refer
  " to state. See :help Partial for more information.
  let cbs.exit_cb = function('s:exit_cb', [], state)

  " explicitly bind close_cb to state so that within it, self will
  " always refer to state. See :help Partial for more information.
  let cbs.close_cb = function('s:close_cb', [], state)

  function state.show_errors(job, exit_status, data)
    if self.for == '_'
      return
    endif

    let l:winid = win_getid(winnr())
    " Always set the active window to the window that was active when the job
    " was started. Among other things, this makes sure that the correct
    " window's location list will be populated when the list type is
    " 'location' and the user has moved windows since starting the job.
    call win_gotoid(self.winid)

    let l:listtype = go#list#Type(self.for)
    if a:exit_status == 0
      call go#list#Clean(l:listtype)
      call win_gotoid(l:winid)
      return
    endif

    let l:listtype = go#list#Type(self.for)
    if len(a:data) == 0
      call go#list#Clean(l:listtype)
      call win_gotoid(l:winid)
      return
    endif

    let out = join(self.messages, "\n")

    let l:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
    try
      " parse the errors relative to self.jobdir
      execute l:cd self.jobdir
      call go#list#ParseFormat(l:listtype, self.errorformat, out, self.for)
      let errors = go#list#Get(l:listtype)
    finally
      execute l:cd fnameescape(self.dir)
    endtry


    if empty(errors)
      " failed to parse errors, output the original content
      call go#util#EchoError([self.dir] + self.messages)
      call win_gotoid(l:winid)
      return
    endif

    " only open the error window if user was still in the window from which
    " the job was started.
    if self.winid == l:winid
      call go#list#Window(l:listtype, len(errors))
      if !self.bang
        call go#list#JumpToFirst(l:listtype)
      endif
    endif
  endfunction

  return cbs
endfunction

function! s:start(args) dict
  if go#config#EchoCommandInfo() && self.statustype != ""
    let prefix = '[' . self.statustype . '] '
    call go#util#EchoSuccess(prefix . "dispatched")
  endif

  if self.statustype != ''
    let status = {
          \ 'desc': 'current status',
          \ 'type': self.statustype,
          \ 'state': "started",
          \ }

    call go#statusline#Update(self.jobdir, status)
  endif
  let self.started_at = reltime()
endfunction

function! s:callback(chan, msg) dict
  call add(self.messages, a:msg)
endfunction

function! s:exit_cb(job, exitval) dict
  let self.exit_status = a:exitval
  let self.exited = 1

  call self.show_status(a:job, a:exitval)

  if self.closed || has('nvim')
    call self.complete(a:job, self.exit_status, self.messages)
  endif
endfunction

function! s:close_cb(ch) dict
  let self.closed = 1

  if self.exited
    let job = ch_getjob(a:ch)
    call self.complete(job, self.exit_status, self.messages)
  endif
endfunction

" go#job#Start runs a job. The options are expected to be the options
" suitable for Vim8 jobs. When called from Neovim, Vim8 options will be
" transformed to their Neovim equivalents.
function! go#job#Start(cmd, options)
  let l:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let l:options = copy(a:options)

  if has('nvim')
    let l:options = s:neooptions(l:options)
  endif

  " Verify that the working directory for the job actually exists. Return
  " early if the directory does not exist. This helps avoid errors when
  " working with plugins that use virtual files that don't actually exist on
  " the file system.
  let l:filedir = expand("%:p:h")
  if has_key(l:options, 'cwd') && !isdirectory(l:options.cwd)
      return
  elseif !isdirectory(l:filedir)
    return
  endif

  let l:manualcd = 0
  if !has_key(l:options, 'cwd')
    " pre start
    let l:manualcd = 1
    let dir = getcwd()
    execute l:cd fnameescape(filedir)
  elseif !(has("patch-8.0.0902") || has('nvim'))
    let l:manualcd = 1
    let l:dir = l:options.cwd
    execute l:cd fnameescape(l:dir)
    call remove(l:options, 'cwd')
  endif

  if has_key(l:options, '_start')
    call l:options._start()
    " remove _start to play nicely with vim (when vim encounters an unexpected
    " job option it reports an "E475: invalid argument" error).
    unlet l:options._start
  endif

  " noblock was added in 8.1.350; remove it if it's not supported.
  if has_key(l:options, 'noblock') && (has('nvim') || !has("patch-8.1.350"))
    call remove(l:options, 'noblock')
  endif

  if go#util#HasDebug('shell-commands')
    call go#util#EchoInfo('job command: ' . string(a:cmd))
  endif

  if has('nvim')
    let l:input = []
    if has_key(a:options, 'in_io') && a:options.in_io ==# 'file' && !empty(a:options.in_name)
      let l:input = readfile(a:options.in_name, "b")
    endif

    let job = jobstart(a:cmd, l:options)

    if len(l:input) > 0
      call chansend(job, l:input)
      " close stdin to signal that no more bytes will be sent.
      call chanclose(job, 'stdin')
    endif
  else
    let l:cmd = a:cmd
    if go#util#IsWin()
      let l:cmd = join(map(copy(a:cmd), function('s:winjobarg')), " ")
    endif

    let job = job_start(l:cmd, l:options)
  endif

  if l:manualcd
    " post start
    execute l:cd fnameescape(l:dir)
  endif

  return job
endfunction

" s:neooptions returns a dictionary of job options suitable for use by Neovim
" based on a dictionary of job options suitable for Vim8.
function! s:neooptions(options)
  let l:options = {}
  let l:options['stdout_buf'] = ''
  let l:options['stderr_buf'] = ''

  let l:err_mode = get(a:options, 'err_mode', get(a:options, 'mode', ''))
  let l:out_mode = get(a:options, 'out_mode', get(a:options, 'mode', ''))

  for key in keys(a:options)
      if key == 'cwd'
        let l:options['cwd'] = a:options['cwd']
        continue
      endif

      if key == 'callback'
        let l:options['callback'] = a:options['callback']

        if !has_key(a:options, 'out_cb')
          let l:options['on_stdout'] = function('s:callback2on_stdout', [l:out_mode], l:options)
        endif

        if !has_key(a:options, 'err_cb')
          let l:options['on_stderr'] = function('s:callback2on_stderr', [l:err_mode], l:options)
        endif

        continue
      endif

      if key == 'out_cb'
        let l:options['out_cb'] = a:options['out_cb']
        let l:options['on_stdout'] = function('s:on_stdout', [l:out_mode], l:options)

        continue
      endif

      if key == 'err_cb'
        let l:options['err_cb'] = a:options['err_cb']
        let l:options['on_stderr'] = function('s:on_stderr', [l:err_mode], l:options)

        continue
      endif

      if key == 'exit_cb'
        let l:options['exit_cb'] = a:options['exit_cb']
        let l:options['on_exit'] = function('s:on_exit', [], l:options)

        continue
      endif

      if key == 'close_cb'
        continue
      endif

      if key == 'stoponexit'
        if a:options['stoponexit'] == ''
          let l:options['detach'] = 1
        endif
        continue
      endif
  endfor
  return l:options
endfunction

function! s:callback2on_stdout(mode, ch, data, event) dict
  let self.stdout_buf = s:neocb(a:mode, a:ch, self.stdout_buf, a:data, self.callback)
endfunction

function! s:callback2on_stderr(mode, ch, data, event) dict
  let self.stderr_buf = s:neocb(a:mode, a:ch, self.stderr_buf, a:data, self.callback)
endfunction

function! s:on_stdout(mode, ch, data, event) dict
  let self.stdout_buf = s:neocb(a:mode, a:ch, self.stdout_buf, a:data, self.out_cb)
endfunction

function! s:on_stderr(mode, ch, data, event) dict
  let self.stderr_buf = s:neocb(a:mode, a:ch, self.stderr_buf, a:data, self.err_cb )
endfunction

function! s:on_exit(jobid, exitval, event) dict
  call self.exit_cb(a:jobid, a:exitval)
endfunction

function! go#job#Stop(job) abort
  if has('nvim')
    call jobstop(a:job)
    return
  endif

  call job_stop(a:job)
  call go#job#Wait(a:job)
  return
endfunction

function! go#job#Wait(job) abort
  if has('nvim')
    call jobwait([a:job])
    return
  endif

  while job_status(a:job) is# 'run'
    sleep 50m
  endwhile
endfunction

function! s:winjobarg(idx, val) abort
  if empty(a:val)
    return '""'
  endif
  return a:val
endfunction

function! s:neocb(mode, ch, buf, data, callback)
  " dealing with the channel lines of Neovim is awful. The docs (:help
  " channel-lines) say:
  "     stream event handlers may receive partial (incomplete) lines. For a
  "     given invocation of on_stdout etc, `a:data` is not guaranteed to end
  "     with a newline.
  "       - `abcdefg` may arrive as `['abc']`, `['defg']`.
  "       - `abc\nefg` may arrive as `['abc', '']`, `['efg']` or `['abc']`,
  "         `['','efg']`, or even `['ab']`, `['c','efg']`.
  "
  " Thankfully, though, this is explained a bit better in an issue:
  " https://github.com/neovim/neovim/issues/3555. Specifically in these two
  " comments:
  "     * https://github.com/neovim/neovim/issues/3555#issuecomment-152290804
  "     * https://github.com/neovim/neovim/issues/3555#issuecomment-152588749
  "
  " The key is
  "     Every item in the list passed to job control callbacks represents a
  "     string after a newline(Except the first, of course). If the program
  "     outputs: "hello\nworld" the corresponding list is ["hello", "world"].
  "     If the program outputs "hello\nworld\n", the corresponding list is
  "     ["hello", "world", ""]. In other words, you can always determine if
  "     the last line received is complete or not.
  " and
  "     for every list you receive in a callback, all items except the first
  "     represent newlines.

  let l:buf = ''

  " a single empty string means EOF was reached.
  if len(a:data) == 1 && a:data[0] == ''
    " when there's nothing buffered, return early so that an
    " erroneous message will not be added.
    if a:buf == ''
      return ''
    endif

    let l:data = [a:buf]
  else
    let l:data = copy(a:data)
    let l:data[0] = a:buf . l:data[0]

    " The last element may be a partial line; save it for next time.
    if a:mode != 'raw'
      let l:buf = l:data[-1]
      let l:data = l:data[:-2]
    endif
  endif

  let l:i = 0
  let l:last = len(l:data) - 1
  while l:i <= l:last
    let l:msg = l:data[l:i]
    if a:mode == 'raw' && l:i < l:last
      let l:msg = l:msg . "\n"
    endif
    call a:callback(a:ch, l:msg)

    let l:i += 1
  endwhile

  return l:buf
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
