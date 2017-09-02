" Spawn returns callbacks to be used with job_start.  It's abstracted to be
" used with various go command, such as build, test, install, etc.. This avoid
" us to write the same callback over and over for some commands. It's fully
" customizable so each command can change it to it's own logic.
function go#job#Spawn(args)
  let cbs = {
        \ 'winnr': winnr(),
        \ 'dir': getcwd(),
        \ 'jobdir': fnameescape(expand("%:p:h")),
        \ 'messages': [],
        \ 'args': a:args.cmd,
        \ 'bang': 0,
        \ }

  if has_key(a:args, 'bang')
    let cbs.bang = a:args.bang
  endif

  " add final callback to be called if async job is finished
  " The signature should be in form: func(job, exit_status, messages)
  if has_key(a:args, 'custom_cb')
    let cbs.custom_cb = a:args.custom_cb
  endif

  if has_key(a:args, 'error_info_cb')
    let cbs.error_info_cb = a:args.error_info_cb
  endif

  function cbs.callback(chan, msg) dict
    call add(self.messages, a:msg)
  endfunction

  function cbs.exit_cb(job, exitval) dict
    if has_key(self, 'error_info_cb')
      call self.error_info_cb(a:job, a:exitval, self.messages)
    endif

    if get(g:, 'go_echo_command_info', 1)
      if a:exitval == 0
        call go#util#EchoSuccess("SUCCESS")
      else
        call go#util#EchoError("FAILED")
      endif
    endif

    if has_key(self, 'custom_cb')
      call self.custom_cb(a:job, a:exitval, self.messages)
    endif

    let l:listtype = go#list#Type("quickfix")
    if a:exitval == 0
      call go#list#Clean(l:listtype)
      call go#list#Window(l:listtype)
      return
    endif

    call self.show_errors(l:listtype)
  endfunction

  function cbs.show_errors(listtype) dict
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
    try
      execute cd self.jobdir
      let errors = go#tool#ParseErrors(self.messages)
      let errors = go#tool#FilterValids(errors)
    finally
      execute cd . fnameescape(self.dir)
    endtry

    if !len(errors)
      " failed to parse errors, output the original content
      call go#util#EchoError(join(self.messages, " "))
      call go#util#EchoError(self.dir)
      return
    endif

    if self.winnr == winnr()
      call go#list#Populate(a:listtype, errors, join(self.args))
      call go#list#Window(a:listtype, len(errors))
      if !empty(errors) && !self.bang
        call go#list#JumpToFirst(a:listtype)
      endif
    endif
  endfunction

  " override callback handler if user provided it
  if has_key(a:args, 'callback')
    let cbs.callback = a:args.callback
  endif

  " override exit callback handler if user provided it
  if has_key(a:args, 'exit_cb')
    let cbs.exit_cb = a:args.exit_cb
  endif

  return cbs
endfunction
" vim: sw=2 ts=2 et
