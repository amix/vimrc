if !exists('s:log_file')
  let s:log_file = tempname() . '-copilot.log'
  try
    call writefile([], s:log_file)
  catch
  endtry
endif

let s:logs = []

function! copilot#logger#BufReadCmd() abort
  try
    setlocal modifiable noreadonly
    silent call deletebufline('', 1, '$')
    if !empty(s:logs)
      call setline(1, s:logs)
    endif
  finally
    setlocal buftype=nofile bufhidden=wipe nobuflisted nomodified nomodifiable
  endtry
endfunction

let s:level_prefixes = ['', '[ERROR] ', '[WARN] ', '[INFO] ', '[DEBUG] ', '[DEBUG] ']

function! copilot#logger#Raw(level, message) abort
  let lines = type(a:message) == v:t_list ? copy(a:message) : split(a:message, "\n", 1)
  let lines[0] = strftime('[%Y-%m-%d %H:%M:%S] ') . get(s:level_prefixes, a:level, '[UNKNOWN] ') . get(lines, 0, '')
  try
    if !filewritable(s:log_file)
      return
    endif
    call map(lines, { k, L -> type(L) == v:t_func ? call(L, []) : L })
    call extend(s:logs, lines)
    let overflow = len(s:logs) - get(g:, 'copilot_log_history', 10000)
    if overflow > 0
      call remove(s:logs, 0, overflow - 1)
    endif
    let bufnr = bufnr('copilot:///log')
    if bufnr > 0 && bufloaded(bufnr)
      call setbufvar(bufnr, '&modifiable', 1)
      call setbufline(bufnr, 1, s:logs)
      call setbufvar(bufnr, '&modifiable', 0)
      for winid in win_findbuf(bufnr)
        if has('nvim') && winid != win_getid()
          call nvim_win_set_cursor(winid, [len(s:logs), 0])
        endif
      endfor
    endif
  catch
  endtry
endfunction

function! copilot#logger#Debug(...) abort
  if empty(get(g:, 'copilot_debug'))
    return
  endif
  call copilot#logger#Raw(4, a:000)
endfunction

function! copilot#logger#Info(...) abort
  call copilot#logger#Raw(3, a:000)
endfunction

function! copilot#logger#Warn(...) abort
  call copilot#logger#Raw(2, a:000)
endfunction

function! copilot#logger#Error(...) abort
  call copilot#logger#Raw(1, a:000)
endfunction

function! copilot#logger#Bare(...) abort
  call copilot#logger#Raw(0, a:000)
endfunction

function! copilot#logger#Exception(...) abort
  if !empty(v:exception) && v:exception !=# 'Vim:Interrupt'
    call copilot#logger#Error('Exception: ' . v:exception . ' @ ' . v:throwpoint)
    let client = copilot#RunningClient()
    if !empty(client)
      let [_, type, code, message; __] = matchlist(v:exception, '^\%(\(^[[:alnum:]_#]\+\)\%((\a\+)\)\=\%(\(:E-\=\d\+\)\)\=:\s*\)\=\(.*\)$')
      let stacklines = []
      for frame in split(substitute(v:throwpoint, ', \S\+ \(\d\+\)$', '[\1]', ''), '\.\@<!\.\.\.\@!')
        let fn_line = matchlist(frame, '^\%(function \)\=\(\S\+\)\[\(\d\+\)\]$')
        if !empty(fn_line)
          call add(stacklines, {'function': substitute(fn_line[1], '^<SNR>\d\+_', '<SID>', ''), 'lineno': +fn_line[2]})
        elseif frame =~# ' Autocmds for "\*"$'
          call add(stacklines, {'function': frame})
        elseif frame =~# ' Autocmds for ".*"$'
          call add(stacklines, {'function': substitute(frame, ' for ".*"$', ' for "[redacted]"', '')})
        else
          call add(stacklines, {'function': '[redacted]'})
        endif
      endfor
      return client.Request('telemetry/exception', {
            \ 'transaction': a:0 ? a:1 : '',
            \ 'platform': 'other',
            \ 'exception_detail': [{
            \ 'type': type . code,
            \ 'value': message,
            \ 'stacktrace': stacklines}]
            \ }, v:null, function('copilot#util#Nop'))
    endif
  endif
endfunction
