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

  call gitgutter#debug#vim_version()
  call gitgutter#debug#separator()

  call gitgutter#debug#git_version()
  call gitgutter#debug#separator()

  call gitgutter#debug#grep_version()
  call gitgutter#debug#separator()

  call gitgutter#debug#option('updatetime')
  call gitgutter#debug#option('shell')
  call gitgutter#debug#option('shellcmdflag')
  call gitgutter#debug#option('shellpipe')
  call gitgutter#debug#option('shellquote')
  call gitgutter#debug#option('shellredir')
  call gitgutter#debug#option('shellslash')
  call gitgutter#debug#option('shelltemp')
  call gitgutter#debug#option('shelltype')
  call gitgutter#debug#option('shellxescape')
  call gitgutter#debug#option('shellxquote')
endfunction


function! gitgutter#debug#separator()
  call gitgutter#debug#output('')
endfunction

function! gitgutter#debug#vim_version()
  redir => version_info
    silent execute 'version'
  redir END
  call gitgutter#debug#output(split(version_info, '\n')[0:2])
endfunction

function! gitgutter#debug#git_version()
  let v = system(g:gitgutter_git_executable.' --version')
  call gitgutter#debug#output( substitute(v, '\n$', '', '') )
endfunction

function! gitgutter#debug#grep_version()
  let v = system('grep --version')
  call gitgutter#debug#output( substitute(v, '\n$', '', '') )

  let v = system('grep --help')
  call gitgutter#debug#output( substitute(v, '\%x00', '', 'g') )
endfunction

function! gitgutter#debug#option(name)
  if exists('+' . a:name)
    let v = eval('&' . a:name)
    call gitgutter#debug#output(a:name . '=' . v)
    " redir => output
    "   silent execute "verbose set " . a:name . "?"
    " redir END
    " call gitgutter#debug#output(a:name . '=' . output)
  else
    call gitgutter#debug#output(a:name . ' [n/a]')
  end
endfunction

function! gitgutter#debug#output(text)
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

    execute 'redir >> '.s:log_file
      if s:new_log_session
        let s:start = reltime()
        silent echo "\n==== start log session ===="
      endif

      let elapsed = reltimestr(reltime(s:start)).' '
      silent echo ''
      " callers excluding this function
      silent echo elapsed.expand('<sfile>')[:-22].':'
      silent echo elapsed.s:format_for_log(a:message)
      if a:0 && !empty(a:1)
        for msg in a:000
          silent echo elapsed.s:format_for_log(msg)
        endfor
      endif
    redir END

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

