" Spawn returns callbacks to be used with job_start. It is abstracted to be
" used with various go commands, such as build, test, install, etc.. This
" allows us to avoid writing the same callback over and over for some
" commands. It's fully customizable so each command can change it to it's own
" logic.
"
" args is a dictionary with the these keys:
"   'cmd':
"     The value to pass to job_start().
"   'bang':
"     Set to 0 to jump to the first error in the error list.
"     Defaults to 0.
"   'for':
"     The g:go_list_type_command key to use to get the error list type to use.
"     Defaults to '_job'
"   'complete':
"     A function to call after the job exits and the channel is closed. The
"     function will be passed three arguments: the job, its exit code, and the
"     list of messages received from the channel. The default value will
"     process the messages and manage the error list after the job exits and
"     the channel is closed.

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
function go#job#Spawn(args)
  let cbs = {}
  let state = {
        \ 'winid': win_getid(winnr()),
        \ 'dir': getcwd(),
        \ 'jobdir': fnameescape(expand("%:p:h")),
        \ 'messages': [],
        \ 'args': a:args.cmd,
        \ 'bang': 0,
        \ 'for': "_job",
        \ 'exited': 0,
        \ 'exit_status': 0,
        \ 'closed': 0,
        \ 'errorformat': &errorformat
      \ }

  if has_key(a:args, 'bang')
    let state.bang = a:args.bang
  endif

  if has_key(a:args, 'for')
    let state.for = a:args.for
  endif

  " do nothing in state.complete by default.
  function state.complete(job, exit_status, data)
  endfunction

  if has_key(a:args, 'complete')
    let state.complete = a:args.complete
  endif

  function! s:callback(chan, msg) dict
    call add(self.messages, a:msg)
  endfunction
  " explicitly bind callback to state so that within it, self will
  " always refer to state. See :help Partial for more information.
  let cbs.callback = function('s:callback', [], state)

  function! s:exit_cb(job, exitval) dict
    let self.exit_status = a:exitval
    let self.exited = 1

    if go#config#EchoCommandInfo()
      if a:exitval == 0
        call go#util#EchoSuccess("SUCCESS")
      else
        call go#util#EchoError("FAILED")
      endif
    endif

    if self.closed
      call self.complete(a:job, self.exit_status, self.messages)
      call self.show_errors(a:job, self.exit_status, self.messages)
    endif
  endfunction
  " explicitly bind exit_cb to state so that within it, self will always refer
  " to state. See :help Partial for more information.
  let cbs.exit_cb = function('s:exit_cb', [], state)

  function! s:close_cb(ch) dict
    let self.closed = 1

    if self.exited
      let job = ch_getjob(a:ch)
      call self.complete(job, self.exit_status, self.messages)
      call self.show_errors(job, self.exit_status, self.messages)
    endif
  endfunction
  " explicitly bind close_cb to state so that within it, self will
  " always refer to state. See :help Partial for more information.
  let cbs.close_cb = function('s:close_cb', [], state)

  function state.show_errors(job, exit_status, data)
    let l:winid = win_getid(winnr())
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

    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
    try
      " parse the errors relative to self.jobdir
      execute cd self.jobdir
      call go#list#ParseFormat(l:listtype, self.errorformat, out, self.for)
      let errors = go#list#Get(l:listtype)
    finally
      execute cd . fnameescape(self.dir)
    endtry


    if empty(errors)
      " failed to parse errors, output the original content
      call go#util#EchoError(self.messages + [self.dir])
      call win_gotoid(l:winid)
      return
    endif

    if self.winid == l:winid
      call go#list#Window(l:listtype, len(errors))
      if !self.bang
        call go#list#JumpToFirst(l:listtype)
      endif
    endif
  endfunction

  return cbs
endfunction

" vim: sw=2 ts=2 et
