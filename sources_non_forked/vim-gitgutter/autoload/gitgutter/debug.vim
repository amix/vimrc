let s:plugin_dir  = expand('<sfile>:p:h:h:h').'/'
let s:log_file    = s:plugin_dir.'gitgutter.log'
let s:channel_log = s:plugin_dir.'channel.log'
let s:new_log_session = 1


function! gitgutter#debug#debug()
  " Open a scratch buffer
  vsplit __GitGutter_Debug__
  normal! ggdG
  setlocal buftype=nofile
  setlocal bufhidden=delete
  setlocal noswapfile

  call s:vim_version()
  call s:separator()

  call s:git_version()
  call s:separator()

  call s:grep_version()
  call s:separator()

  call s:option('updatetime')
endfunction


function! s:separator()
  call s:output('')
endfunction

function! s:vim_version()
  redir => version_info
    silent execute 'version'
  redir END
  call s:output(split(version_info, '\n')[0:2])
endfunction

function! s:git_version()
  let v = system(g:gitgutter_git_executable.' --version')
  call s:output( substitute(v, '\n$', '', '') )
endfunction

function! s:grep_version()
  let v = system(g:gitgutter_grep.' --version')
  call s:output( substitute(v, '\n$', '', '') )

  let v = system(g:gitgutter_grep.' --help')
  call s:output( substitute(v, '\%x00', '', 'g') )
endfunction

function! s:option(name)
  if exists('+' . a:name)
    let v = eval('&' . a:name)
    call s:output(a:name . '=' . v)
    " redir => output
    "   silent execute "verbose set " . a:name . "?"
    " redir END
    " call s:output(a:name . '=' . output)
  else
    call s:output(a:name . ' [n/a]')
  end
endfunction

function! s:output(text)
  call append(line('$'), a:text)
endfunction

" assumes optional args are calling function's optional args
function! gitgutter#debug#log(message, ...) abort
  if g:gitgutter_log
    if s:new_log_session && gitgutter#async#available()
      if exists('*ch_logfile')
        call ch_logfile(s:channel_log, 'w')
      endif
    endif

    if s:new_log_session
      let s:start = reltime()
      call writefile(['', '========== start log session '.strftime('%d.%m.%Y %H:%M:%S').' =========='], s:log_file, 'a')
    endif

    let elapsed = reltimestr(reltime(s:start)).' '
    call writefile([''], s:log_file, 'a')
    " callers excluding this function
    call writefile([elapsed.expand('<sfile>')[:-22].':'], s:log_file, 'a')
    call writefile([elapsed.s:format_for_log(a:message)], s:log_file, 'a')
    if a:0 && !empty(a:1)
      for msg in a:000
        call writefile([elapsed.s:format_for_log(msg)], s:log_file, 'a')
      endfor
    endif

    let s:new_log_session = 0
  endif
endfunction

function! s:format_for_log(data) abort
  if type(a:data) == 1
    return join(split(a:data,'\n'),"\n")
  elseif type(a:data) == 3
    return '['.join(a:data,"\n").']'
  else
    return a:data
  endif
endfunction

