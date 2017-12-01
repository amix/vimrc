let s:available = has('nvim') || (
      \   has('job') && (
      \     (has('patch-7-4-1826') && !has('gui_running')) ||
      \     (has('patch-7-4-1850') &&  has('gui_running')) ||
      \     (has('patch-7-4-1832') &&  has('gui_macvim'))
      \   )
      \ )

function! gitgutter#async#available()
  return s:available
endfunction


function! gitgutter#async#execute(cmd) abort
  let options = {
        \   'stdoutbuffer': [],
        \   'buffer': gitgutter#utility#bufnr()
        \ }
  let command = s:build_command(a:cmd)

  if has('nvim')
    call jobstart(command, extend(options, {
          \   'on_stdout': function('s:on_stdout_nvim'),
          \   'on_stderr': function('s:on_stderr_nvim'),
          \   'on_exit':   function('s:on_exit_nvim')
          \ }))
  else
    call job_start(command, {
          \   'out_cb':   function('s:on_stdout_vim', options),
          \   'err_cb':   function('s:on_stderr_vim', options),
          \   'close_cb': function('s:on_exit_vim', options)
          \ })
  endif
endfunction


function! s:build_command(cmd)
  if has('unix')
    return ['sh', '-c', a:cmd]
  endif

  if has('win32')
    return has('nvim') ? ['cmd.exe', '/c', a:cmd] : 'cmd.exe /c '.a:cmd
  endif

  throw 'unknown os'
endfunction


function! s:on_stdout_nvim(_job_id, data, _event) dict abort
  if empty(self.stdoutbuffer)
    let self.stdoutbuffer = a:data
  else
    let self.stdoutbuffer = self.stdoutbuffer[:-2] +
          \ [self.stdoutbuffer[-1] . a:data[0]] +
          \ a:data[1:]
  endif
endfunction

function! s:on_stderr_nvim(_job_id, _data, _event) dict abort
  " Backward compatibility for nvim < 0.2.0
  if !has('nvim-0.2.0')
    let current_buffer = gitgutter#utility#bufnr()
    call gitgutter#utility#set_buffer(self.buffer)
    if gitgutter#utility#is_active()
      call gitgutter#hunk#reset()
    endif
    call gitgutter#utility#set_buffer(current_buffer)
    return
  endif

  call s:buffer_exec(self.buffer, function('gitgutter#hunk#reset'))
endfunction

function! s:on_exit_nvim(_job_id, _data, _event) dict abort
  " Backward compatibility for nvim < 0.2.0
  if !has('nvim-0.2.0')
    let current_buffer = gitgutter#utility#bufnr()
    call gitgutter#utility#set_buffer(self.buffer)
    if gitgutter#utility#is_active()
      call gitgutter#handle_diff(gitgutter#utility#stringify(self.stdoutbuffer))
    endif
    call gitgutter#utility#set_buffer(current_buffer)
    return
  endif

  call s:buffer_exec(self.buffer, function('gitgutter#handle_diff', [gitgutter#utility#stringify(self.stdoutbuffer)]))
endfunction


function! s:on_stdout_vim(_channel, data) dict abort
  call add(self.stdoutbuffer, a:data)
endfunction

function! s:on_stderr_vim(_channel, _data) dict abort
  call s:buffer_exec(self.buffer, function('gitgutter#hunk#reset'))
endfunction

function! s:on_exit_vim(_channel) dict abort
  call s:buffer_exec(self.buffer, function('gitgutter#handle_diff', [gitgutter#utility#stringify(self.stdoutbuffer)]))
endfunction


function! s:buffer_exec(buffer, fn)
  let current_buffer = gitgutter#utility#bufnr()
  call gitgutter#utility#set_buffer(a:buffer)

  if gitgutter#utility#is_active()
    call a:fn()
  endif

  call gitgutter#utility#set_buffer(current_buffer)
endfunction
