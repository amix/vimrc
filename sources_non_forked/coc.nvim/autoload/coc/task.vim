" ============================================================================
" Description: Manage long running tasks.
" Author: Qiming Zhao <chemzqm@gmail.com>
" Licence: Anti 966 licence
" Version: 0.1
" Last Modified:  Dec 12, 2020
" ============================================================================
scriptencoding utf-8

let s:is_vim = !has('nvim')
let s:running_task = {}
" neovim emit strings that part of lines.
let s:out_remain_text = {}
let s:err_remain_text = {}

function! coc#task#start(id, opts)
  if coc#task#running(a:id)
    call coc#task#stop(a:id)
  endif
  let cmd = [a:opts['cmd']] + get(a:opts, 'args', [])
  let cwd = get(a:opts, 'cwd', getcwd())
  let env = get(a:opts, 'env', {})
  " cmd args cwd pty
  if s:is_vim
    let options = {
          \ 'cwd': cwd,
          \ 'err_mode': 'nl',
          \ 'out_mode': 'nl',
          \ 'err_cb': {channel, message -> s:on_stderr(a:id, [message])},
          \ 'out_cb': {channel, message -> s:on_stdout(a:id, [message])},
          \ 'exit_cb': {channel, code -> s:on_exit(a:id, code)},
          \ 'env': env,
          \}
    if has("patch-8.1.350")
      let options['noblock'] = 1
    endif
    if get(a:opts, 'pty', 0)
      let options['pty'] = 1
    endif
    let job = job_start(cmd, options)
    let status = job_status(job)
    if status !=# 'run'
      echohl Error | echom 'Failed to start '.a:id.' task' | echohl None
      return v:false
    endif
    let s:running_task[a:id] = job
  else
    let options = {
          \ 'cwd': cwd,
          \ 'on_stderr': {channel, msgs -> s:on_stderr(a:id, msgs)},
          \ 'on_stdout': {channel, msgs -> s:on_stdout(a:id, msgs)},
          \ 'on_exit': {channel, code -> s:on_exit(a:id, code)},
          \ 'detach': get(a:opts, 'detach', 0),
          \}
    let original = {}
    if !empty(env)
      if has('nvim-0.5.0')
        let options['env'] = env
      elseif exists('*setenv') && exists('*getenv')
        for key in keys(env)
          let original[key] = getenv(key)
          call setenv(key, env[key])
        endfor
      endif
    endif
    if get(a:opts, 'pty', 0)
      let options['pty'] = 1
    endif
    let chan_id = jobstart(cmd, options)
    if !empty(original)
      for key in keys(original)
        call setenv(key, original[key])
      endfor
    endif
    if chan_id <= 0
      echohl Error | echom 'Failed to start '.a:id.' task' | echohl None
      return v:false
    endif
    let s:running_task[a:id] = chan_id
  endif
  return v:true
endfunction

function! coc#task#stop(id)
  let job = get(s:running_task, a:id, v:null)
  if !job | return | endif
  if s:is_vim
    call job_stop(job, 'term')
  else
    call jobstop(job)
  endif
  sleep 50m
  let running = coc#task#running(a:id)
  if running
    echohl Error | echom 'job '.a:id. ' stop failed.' | echohl None
  endif
endfunction

function! s:on_exit(id, code) abort
  if get(g:, 'coc_vim_leaving', 0) | return | endif
  if has('nvim')
    let s:out_remain_text[a:id] = ''
    let s:err_remain_text[a:id] = ''
  endif
  if has_key(s:running_task, a:id)
    call remove(s:running_task, a:id)
  endif
  call coc#rpc#notify('TaskExit', [a:id, a:code])
endfunction

function! s:on_stderr(id, msgs)
  if get(g:, 'coc_vim_leaving', 0) | return | endif
  if empty(a:msgs)
    return
  endif
  if s:is_vim
    call coc#rpc#notify('TaskStderr', [a:id, a:msgs])
  else
    let remain = get(s:err_remain_text, a:id, '')
    let eof = (a:msgs == [''])
    let msgs = copy(a:msgs)
    if len(remain) > 0
      if msgs[0] == ''
        let msgs[0] = remain
      else
        let msgs[0] = remain . msgs[0]
      endif
    endif
    let last = msgs[len(msgs) - 1]
    let s:err_remain_text[a:id] = len(last) > 0 ? last : ''
    " all lines from 0 to n - 2
    if len(msgs) > 1
      call coc#rpc#notify('TaskStderr', [a:id, msgs[:len(msgs)-2]])
    elseif eof && len(msgs[0]) > 0
      call coc#rpc#notify('TaskStderr', [a:id, msgs])
    endif
  endif
endfunction

function! s:on_stdout(id, msgs)
  if empty(a:msgs)
    return
  endif
  if s:is_vim
    call coc#rpc#notify('TaskStdout', [a:id, a:msgs])
  else
    let remain = get(s:out_remain_text, a:id, '')
    let eof = (a:msgs == [''])
    let msgs = copy(a:msgs)
    if len(remain) > 0
      if msgs[0] == ''
        let msgs[0] = remain
      else
        let msgs[0] = remain . msgs[0]
      endif
    endif
    let last = msgs[len(msgs) - 1]
    let s:out_remain_text[a:id] = len(last) > 0 ? last : ''
    " all lines from 0 to n - 2
    if len(msgs) > 1
      call coc#rpc#notify('TaskStdout', [a:id, msgs[:len(msgs)-2]])
    elseif eof && len(msgs[0]) > 0
      call coc#rpc#notify('TaskStdout', [a:id, msgs])
    endif
  endif
endfunction

function! coc#task#running(id)
  if !has_key(s:running_task, a:id) == 1
    return v:false
  endif
  let job = s:running_task[a:id]
  if s:is_vim
    let status = job_status(job)
    return status ==# 'run'
  endif
  let [code] = jobwait([job], 10)
  return code == -1
endfunction
