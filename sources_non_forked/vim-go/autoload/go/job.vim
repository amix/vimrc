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
"   'callback':
"     A function to call when there is a message to read from the job's
"     channel. The function will be passed two arguments: the channel and a
"     message. See job-callback.

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

  let winnr = winnr()
  let dir = getcwd()
  let jobdir = fnameescape(expand("%:p:h"))
  let messages = []
  let args = a:args.cmd
  let bang = 0
  let for = "_job"

  if has_key(a:args, 'bang')
    let l:bang = a:args.bang
  endif

  if has_key(a:args, 'for')
    let l:for = a:args.for
  endif

  let l:exited = 0
  let l:exit_status = 0
  let l:closed = 0

  function! s:NopComplete(job, exit_status, data)
  endfunction

  let Complete = funcref('s:NopComplete')

  if has_key(a:args, 'complete')
    let Complete = a:args.complete
  endif

  function cbs.callback(chan, msg) dict closure
    call add(messages, a:msg)
  endfunction

  function cbs.exit_cb(job, exitval) dict closure
    let exit_status = a:exitval
    let exited = 1

    if get(g:, 'go_echo_command_info', 1)
      if a:exitval == 0
        call go#util#EchoSuccess("SUCCESS")
      else
        call go#util#EchoError("FAILED")
      endif
    endif

    if closed
      call Complete(a:job, exit_status, messages)
      call s:show_errors(a:job, exit_status, messages)
    endif
  endfunction

  function cbs.close_cb(ch) dict closure
    let closed = 1

    if exited
      let job = ch_getjob(a:ch)
      call Complete(job, exit_status, messages)
      call s:show_errors(job, exit_status, messages)
    endif
  endfunction

  function! s:show_errors(job, exit_status, data) closure
    let l:listtype = go#list#Type(for)
    if a:exit_status == 0
      call go#list#Clean(l:listtype)
      call go#list#Window(l:listtype)
      return
    endif

    let l:listtype = go#list#Type(for)
    if len(a:data) == 0
      call go#list#Clean(l:listtype)
      call go#list#Window(l:listtype)
      return
    endif

    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
    try
      execute cd jobdir
      let errors = go#tool#ParseErrors(a:data)
      let errors = go#tool#FilterValids(errors)
    finally
      execute cd . fnameescape(dir)
    endtry

    if empty(errors)
      " failed to parse errors, output the original content
      call go#util#EchoError(messages + [dir])
      return
    endif

    if winnr == winnr()
      call go#list#Populate(l:listtype, errors, join(args))
      call go#list#Window(l:listtype, len(errors))
      if !bang
        call go#list#JumpToFirst(l:listtype)
      endif
    endif
  endfunction

  return cbs
endfunction

" vim: sw=2 ts=2 et
