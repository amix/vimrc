scriptencoding utf-8
let s:is_vim = !has('nvim')
let s:channel_map = {}
let s:is_win = has('win32') || has('win64')

" start terminal, return [bufnr, pid]
function! coc#terminal#start(cmd, cwd, env, strict) abort
  if s:is_vim && !has('terminal')
    throw 'terminal feature not supported by current vim.'
  endif
  let cwd = empty(a:cwd) ? getcwd() : a:cwd
  execute 'belowright '.get(g:, 'coc_terminal_height', 8).'new +setl\ buftype=nofile'
  setl winfixheight
  setl norelativenumber
  setl nonumber
  setl bufhidden=hide
  if exists('#User#CocTerminalOpen')
    exe 'doautocmd <nomodeline> User CocTerminalOpen'
  endif
  let bufnr = bufnr('%')
  let env = {}
  let original = {}
  if !empty(a:env)
    " use env option when possible
    if s:is_vim
      let env = copy(a:env)
    elseif exists('*setenv')
      for key in keys(a:env)
        let original[key] = getenv(key)
        call setenv(key, a:env[key])
      endfor
    endif
  endif

  function! s:OnExit(status) closure
    call coc#rpc#notify('CocAutocmd', ['TermExit', bufnr, a:status])
    if a:status == 0
      execute 'silent! bd! '.bufnr
    endif
  endfunction

  if has('nvim')
    let job_id = termopen(a:cmd, {
          \ 'cwd': cwd,
          \ 'pty': v:true,
          \ 'on_exit': {job, status -> s:OnExit(status)},
          \ 'env': env,
          \ 'clear_env': a:strict ? v:true : v:false
          \ })
    if !empty(original) && exists('*setenv')
      for key in keys(original)
        call setenv(key, original[key])
      endfor
    endif
    if job_id == 0
      throw 'create terminal job failed'
    endif
    wincmd p
    let s:channel_map[bufnr] = job_id
    return [bufnr, jobpid(job_id)]
  else
    let cmd = s:is_win ? join(a:cmd, ' ') : a:cmd
    let res = term_start(cmd, {
          \ 'cwd': cwd,
          \ 'term_kill': s:is_win ? 'kill' : 'term',
          \ 'term_finish': 'close',
          \ 'exit_cb': {job, status -> s:OnExit(status)},
          \ 'curwin': 1,
          \ 'env': env,
          \})
    if res == 0
      throw 'create terminal job failed'
    endif
    let job = term_getjob(bufnr)
    let s:channel_map[bufnr] = job_getchannel(job)
    wincmd p
    return [bufnr, job_info(job).process]
  endif
endfunction

function! coc#terminal#send(bufnr, text, add_new_line) abort
  let chan = get(s:channel_map, a:bufnr, v:null)
  if empty(chan) | return| endif
  if has('nvim')
    let lines = split(a:text, '\v\r?\n')
    if a:add_new_line && !empty(lines[len(lines) - 1])
      if s:is_win
        call add(lines, "\r\n")
      else
        call add(lines, '')
      endif
    endif
    call chansend(chan, lines)
    let winid = bufwinid(a:bufnr)
    if winid != -1
      call coc#compat#execute(winid, 'noa normal! G')
    endif
  else
    if !a:add_new_line
      call ch_sendraw(chan, a:text)
    else
      call ch_sendraw(chan, a:text.(s:is_win ? "\r\n" : "\n"))
    endif
  endif
endfunction

function! coc#terminal#close(bufnr) abort
  if has('nvim')
    let job_id = get(s:channel_map, a:bufnr, 0)
    if !empty(job_id)
      silent! call chanclose(job_id)
    endif
  endif
  exe 'silent! bd! '.a:bufnr
endfunction
