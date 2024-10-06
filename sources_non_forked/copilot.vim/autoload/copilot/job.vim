scriptencoding utf-8

function! copilot#job#Nop(...) abort
endfunction

function! s:Jobs(job_or_jobs) abort
  let jobs = type(a:job_or_jobs) == v:t_list ? copy(a:job_or_jobs) : [a:job_or_jobs]
  call map(jobs, { k, v -> type(v) == v:t_dict ? get(v, 'job', '') : v })
  call filter(jobs, { k, v -> type(v) !=# type('') })
  return jobs
endfunction

let s:job_stop = exists('*job_stop') ? 'job_stop' : 'jobstop'
function! copilot#job#Stop(job) abort
  for job in s:Jobs(a:job)
    call call(s:job_stop, [job])
  endfor
  return copilot#job#Wait(a:job)
endfunction

let s:sleep = has('patch-8.2.2366') ? 'sleep! 1m' : 'sleep 1m'
function! copilot#job#Wait(jobs) abort
  let jobs = s:Jobs(a:jobs)
  if exists('*jobwait')
    call jobwait(jobs)
  else
    for job in jobs
      while ch_status(job) !=# 'closed' || job_status(job) ==# 'run'
        exe s:sleep
      endwhile
    endfor
  endif
  return a:jobs
endfunction

function! s:VimExitCallback(result, exit_cb, job, data) abort
  let a:result.exit_status = a:data
  if !has_key(a:result, 'closed')
    return
  endif
  call remove(a:result, 'closed')
  call a:exit_cb(a:result.exit_status)
endfunction

function! s:VimCloseCallback(result, exit_cb, job) abort
  if !has_key(a:result, 'exit_status')
    let a:result.closed = v:true
    return
  endif
  call a:exit_cb(a:result.exit_status)
endfunction

function! s:NvimCallback(cb, job, data, type) dict abort
  let self[a:type][0] .= remove(a:data, 0)
  call extend(self[a:type], a:data)
  while len(self[a:type]) > 1
    call a:cb(substitute(remove(self[a:type], 0), "\r$", '', ''))
  endwhile
endfunction

function! s:NvimExitCallback(out_cb, err_cb, exit_cb, job, data, type) dict abort
  if len(self.stderr[0])
    call a:err_cb(substitute(self.stderr[0], "\r$", '', ''))
  endif
  call a:exit_cb(a:data)
endfunction

function! copilot#job#Cwd() abort
  let home = expand("~")
  if !isdirectory(home) && isdirectory($VIM)
    return $VIM
  endif
  return home
endfunction

function! copilot#job#Stream(argv, out_cb, err_cb, ...) abort
  let exit_status = []
  let ExitCb = function(a:0 && !empty(a:1) ? a:1 : { e -> add(exit_status, e) }, a:000[2:-1])
  let OutCb = function(empty(a:out_cb) ? 'copilot#job#Nop' : a:out_cb, a:000[2:-1])
  let ErrCb = function(empty(a:err_cb) ? 'copilot#job#Nop' : a:err_cb, a:000[2:-1])
  let state = {'headers': {}, 'mode': 'headers', 'buffer': ''}
  if exists('*job_start')
    let result = {}
    let job = job_start(a:argv, {
          \ 'cwd': copilot#job#Cwd(),
          \ 'out_mode': 'raw',
          \ 'out_cb': { j, d -> OutCb(d) },
          \ 'err_cb': { j, d -> ErrCb(d) },
          \ 'exit_cb': function('s:VimExitCallback', [result, ExitCb]),
          \ 'close_cb': function('s:VimCloseCallback', [result, ExitCb]),
          \ })
  else
    let jopts = {
          \ 'cwd': copilot#job#Cwd(),
          \ 'stderr': [''],
          \ 'on_stdout': { j, d, t -> OutCb(join(d, "\n")) },
          \ 'on_stderr': function('s:NvimCallback', [ErrCb]),
          \ 'on_exit': function('s:NvimExitCallback', [OutCb, ErrCb, ExitCb])}
    let job = jobstart(a:argv, jopts)
  endif
  if a:0
    return job
  endif
  call copilot#job#Wait(job)
  return exit_status[0]
endfunction
