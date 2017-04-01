let s:jobs = {}

" Nvim has always supported async commands.
"
" Vim introduced async in 7.4.1826.
"
" gVim didn't support aync until 7.4.1850 (though I haven't been able to
" verify this myself).
"
" MacVim-GUI didn't support async until 7.4.1832 (actually commit
" 88f4fe0 but 7.4.1832 was the first subsequent patch release).
let s:available = has('nvim') || (
      \ has('job') && (
		  \ (has('patch-7-4-1826') && !has('gui_running')) ||
		  \ (has('patch-7-4-1850') &&  has('gui_running')) ||
		  \ (has('patch-7-4-1832') &&  has('gui_macvim'))
		  \ )
      \ )

function! gitgutter#async#available()
  return s:available
endfunction

function! gitgutter#async#execute(cmd) abort
  let bufnr = gitgutter#utility#bufnr()

  if has('nvim')
    if has('unix')
      let command = ["sh", "-c", a:cmd]
    elseif has('win32')
      let command = ["cmd.exe", "/c", a:cmd]
    else
      throw 'unknown os'
    endif
    " Make the job use a shell while avoiding (un)quoting problems.
    let job_id = jobstart(command, {
          \ 'buffer':    bufnr,
          \ 'on_stdout': function('gitgutter#async#handle_diff_job_nvim'),
          \ 'on_stderr': function('gitgutter#async#handle_diff_job_nvim'),
          \ 'on_exit':   function('gitgutter#async#handle_diff_job_nvim')
          \ })
    call gitgutter#debug#log('[nvim job: '.job_id.', buffer: '.bufnr.'] '.a:cmd)
    if job_id < 1
      throw 'diff failed'
    endif

    " Note that when `cmd` doesn't produce any output, i.e. the diff is empty,
    " the `stdout` event is not fired on the job handler.  Therefore we keep
    " track of the jobs ourselves so we can spot empty diffs.
    call s:job_started(job_id)

  else
    " Make the job use a shell.
    "
    " Pass a handler for stdout but not for stderr so that errors are
    " ignored (and thus signs are not updated; this assumes that an error
    " only occurs when a file is not tracked by git).

    if has('unix')
      let command = ["sh", "-c", a:cmd]
    elseif has('win32')
      let command = "cmd.exe /c ".a:cmd
    else
      throw 'unknown os'
    endif

    let job = job_start(command, {
          \ 'out_cb':   'gitgutter#async#handle_diff_job_vim',
          \ 'close_cb': 'gitgutter#async#handle_diff_job_vim_close'
          \ })
    call gitgutter#debug#log('[vim job: '.string(job_info(job)).', buffer: '.bufnr.'] '.a:cmd)

    call s:job_started(s:channel_id(job_getchannel(job)), bufnr)
  endif
endfunction


function! gitgutter#async#handle_diff_job_nvim(job_id, data, event) dict abort
  call gitgutter#debug#log('job_id: '.a:job_id.', event: '.a:event.', buffer: '.self.buffer)

  let job_bufnr = self.buffer
  if bufexists(job_bufnr)
    let current_buffer = gitgutter#utility#bufnr()
    call gitgutter#utility#set_buffer(job_bufnr)

    if a:event == 'stdout'
      " a:data is a list
      call s:job_finished(a:job_id)
      if gitgutter#utility#is_active()
        call gitgutter#handle_diff(gitgutter#utility#stringify(a:data))
      endif

    elseif a:event == 'exit'
      " If the exit event is triggered without a preceding stdout event,
      " the diff was empty.
      if s:is_job_started(a:job_id)
        if gitgutter#utility#is_active()
          call gitgutter#handle_diff("")
        endif
        call s:job_finished(a:job_id)
      endif

    else  " a:event is stderr
      call gitgutter#hunk#reset()
      call s:job_finished(a:job_id)

    endif

    call gitgutter#utility#set_buffer(current_buffer)
  else
    call s:job_finished(a:job_id)
  endif
endfunction


" Channel is in NL mode.
function! gitgutter#async#handle_diff_job_vim(channel, line) abort
  call gitgutter#debug#log('channel: '.a:channel.', line: '.a:line)

  call s:accumulate_job_output(s:channel_id(a:channel), a:line)
endfunction

function! gitgutter#async#handle_diff_job_vim_close(channel) abort
  call gitgutter#debug#log('channel: '.a:channel)

  let channel_id = s:channel_id(a:channel)
  let job_bufnr = s:job_buffer(channel_id)

  if bufexists(job_bufnr)
    let current_buffer = gitgutter#utility#bufnr()
    call gitgutter#utility#set_buffer(job_bufnr)

    if gitgutter#utility#is_active()
      call gitgutter#handle_diff(s:job_output(channel_id))
    endif

    call gitgutter#utility#set_buffer(current_buffer)
  endif
  call s:job_finished(channel_id)
endfunction


function! s:channel_id(channel) abort
  return ch_info(a:channel)['id']
endfunction


"
" Keep track of jobs.
"
" nvim: receives all the job's output at once so we don't need to accumulate
" it ourselves.  We can pass the buffer number into the job so we don't need
" to track that either.
"
"   s:jobs {} -> key: job's id, value: anything truthy
"
" vim: receives the job's output line by line so we need to accumulate it.
" We also need to keep track of the buffer the job is running for.
" Vim job's don't have an id.  Instead we could use the external process's id
" or the channel's id (there seems to be 1 channel per job).  Arbitrarily
" choose the channel's id.
"
"   s:jobs {} -> key: channel's id, value: {} key: output, value: [] job's output
"                                             key: buffer: value: buffer number


" nvim:
"        id: job's id
"
" vim:
"        id: channel's id
"        arg: buffer number
function! s:job_started(id, ...) abort
  if a:0  " vim
    let s:jobs[a:id] = {'output': [], 'buffer': a:1}
  else    " nvim
    let s:jobs[a:id] = 1
  endif
endfunction

function! s:is_job_started(id) abort
  return has_key(s:jobs, a:id)
endfunction

function! s:accumulate_job_output(id, line) abort
  call add(s:jobs[a:id].output, a:line)
endfunction

" Returns a string
function! s:job_output(id) abort
  if has_key(s:jobs, a:id)
    return gitgutter#utility#stringify(s:jobs[a:id].output)
  else
    return ""
  endif
endfunction

function! s:job_buffer(id) abort
  return s:jobs[a:id].buffer
endfunction

function! s:job_finished(id) abort
  if has_key(s:jobs, a:id)
    unlet s:jobs[a:id]
  endif
endfunction

