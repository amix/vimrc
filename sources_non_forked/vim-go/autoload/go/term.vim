" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let s:bufnameprefix = 'goterm://'

" new creates a new terminal with the given command. Mode is set based on the
" global variable g:go_term_mode, which is by default set to :vsplit
function! go#term#new(bang, cmd, errorformat) abort
  return go#term#newmode(a:bang, a:cmd, a:errorformat, go#config#TermMode())
endfunction

" go#term#newmode creates a new terminal with the given command and window mode.
function! go#term#newmode(bang, cmd, errorformat, mode) abort
  let l:mode = a:mode
  if empty(l:mode)
    let l:mode = go#config#TermMode()
  endif

  if go#config#TermReuse()
    call s:closeterm()
  endif

  let l:state = {
        \ 'cmd': a:cmd,
        \ 'bang' : a:bang,
        \ 'winid': win_getid(winnr()),
        \ 'stdout': [],
        \ 'stdout_buf': '',
        \ 'errorformat': a:errorformat,
      \ }

  " execute the command in the current file's directory
  let l:dir = go#util#Chdir(expand('%:p:h'))

  execute l:mode . ' __go_term__'
  setlocal filetype=goterm
  setlocal bufhidden=delete
  setlocal winfixheight
  " TODO(bc)?: setlocal winfixwidth
  setlocal noswapfile
  setlocal nobuflisted

  " setup job for nvim
  if has('nvim')
    " explicitly bind callbacks to state so that within them, self will always
    " refer to state. See :help Partial for more information.
    "
    " Don't set an on_stderr, because it will be passed the same data as
    " on_stdout. See https://github.com/neovim/neovim/issues/2836
    let l:job = {
          \ 'on_stdout': function('s:on_stdout', [], state),
          \ 'on_exit' : function('s:on_exit', [], state),
        \ }
    let l:state.id = termopen(a:cmd, l:job)
    let l:state.termwinid = win_getid(winnr())
    let s:lasttermwinid = l:state.termwinid
    call go#util#Chdir(l:dir)

    " resize new term if needed.
    let l:height = go#config#TermHeight()
    let l:width = go#config#TermWidth()

    " Adjust the window width or height depending on whether it's a vertical or
    " horizontal split.
    if l:mode =~ "vertical" || l:mode =~ "vsplit" || l:mode =~ "vnew"
      exe 'vertical resize ' . l:width
    elseif mode =~ "split" || mode =~ "new"
      exe 'resize ' . l:height
    endif
    " we also need to resize the pty, so there you go...
    call jobresize(l:state.id, l:width, l:height)

  " setup term for vim8
  elseif has('terminal')
    " Not great randomness, but "good enough" for our purpose here.
    let l:rnd = sha256(printf('%s%s', reltimestr(reltime()), fnamemodify(bufname(''), ":p")))
    let l:termname = printf("%s%s", s:bufnameprefix, l:rnd)

    let l:term = {
          \ 'out_cb': function('s:out_cb', [], state),
          \ 'exit_cb' : function('s:exit_cb', [], state),
          \ 'curwin': 1,
          \ 'term_name': l:termname,
        \ }

    if l:mode =~ "vertical" || l:mode =~ "vsplit" || l:mode =~ "vnew"
      let l:term["vertical"] = l:mode
    endif

    let l:state.id = term_start(a:cmd, l:term)
    let l:state.termwinid = win_getid(bufwinnr(l:state.id))
    let s:lasttermwinid = l:state.termwinid
    call go#util#Chdir(l:dir)

    " resize new term if needed.
    let l:height = go#config#TermHeight()
    let l:width = go#config#TermWidth()

    " Adjust the window width or height depending on whether it's a vertical or
    " horizontal split.
    if l:mode =~ "vertical" || l:mode =~ "vsplit" || l:mode =~ "vnew"
      exe 'vertical resize ' . l:width
    elseif mode =~ "split" || mode =~ "new"
      exe 'resize ' . l:height
    endif
    "if exists(*term_setsize)
      "call term_setsize(l:state.id, l:height, l:width)
    "endif
  endif

  call win_gotoid(l:state.winid)
  return l:state.id
endfunction

" out_cb continually concat's the self.stdout_buf on recv of stdout
" and sets self.stdout to the new-lined split content in self.stdout_buf
func! s:out_cb(channel, msg) dict abort
  let self.stdout_buf = self.stdout_buf . a:msg
  let self.stdout = split(self.stdout_buf, '\n')
endfunction

function! s:on_stdout(job_id, data, event) dict abort
  " A single empty string means EOF was reached. The first item will never be
  " the empty string except for when it's the only item and is signaling that
  " EOF was reached.
  if len(a:data) == 1 && a:data[0] == ''
    " when there's nothing buffered, return early so that an
    " erroneous message will not be added.
    if self.stdout_buf == ''
      return
    endif

    let self.stdout = add(self.stdout, self.stdout_buf)
  else
    let l:data = copy(a:data)
    let l:data[0] = self.stdout_buf . l:data[0]

    " The last element may be a partial line; save it for next time.
    let self.stdout_buf = l:data[-1]
    let self.stdout = extend(self.stdout, l:data[:-2])
  endif
endfunction

" vim8 exit callback
function! s:exit_cb(job_id, exit_status) dict abort
  call s:handle_exit(a:job_id, a:exit_status, self)
endfunction

" nvim exit callback
function! s:on_exit(job_id, exit_status, event) dict abort
  call s:handle_exit(a:job_id, a:exit_status, self)
endfunction

" handle_exit implements both vim8 and nvim exit callbacks
func s:handle_exit(job_id, exit_status, state) abort
  let l:winid = win_getid(winnr())
  call win_gotoid(a:state.winid)

  let l:listtype = go#list#Type("_term")

  if a:exit_status == 0
    call go#list#Clean(l:listtype)
    call win_gotoid(l:winid)
    return
  endif

  let l:bufdir = expand('%:p:h')
  if !isdirectory(l:bufdir)
    call go#util#EchoWarning('terminal job failure not processed, because the job''s working directory no longer exists')
    call win_gotoid(l:winid)
    return
  endif

  " change to directory where the command was run. If we do not do this the
  " quickfix items will have the incorrect paths.
  " see: https://github.com/fatih/vim-go/issues/2400
  let l:dir = go#util#Chdir(l:bufdir)

  let l:title = a:state.cmd
  if type(l:title) == v:t_list
    let l:title = join(a:state.cmd)
  endif

  let l:i = 0
  while l:i < len(a:state.stdout)
    let a:state.stdout[l:i] = substitute(a:state.stdout[l:i], "\r$", '', 'g')
    let l:i += 1
  endwhile

  call go#list#ParseFormat(l:listtype, a:state.errorformat, a:state.stdout, l:title, 0)
  let l:errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(l:errors))

  " close terminal; we don't need it anymore
  if go#config#TermCloseOnExit()
    call win_gotoid(a:state.termwinid)
    close!
  endif

  if empty(l:errors)
    call go#util#EchoError( '[' . l:title . '] ' . "FAIL")
    call go#util#Chdir(l:dir)
    call win_gotoid(l:winid)
    return
  endif

  if a:state.bang
    call go#util#Chdir(l:dir)
    call win_gotoid(l:winid)
    return
  endif

  call win_gotoid(a:state.winid)
  call go#list#JumpToFirst(l:listtype)

  " change back to original working directory
  call go#util#Chdir(l:dir)
endfunction

function! go#term#ToggleCloseOnExit() abort
  if go#config#TermCloseOnExit()
    call go#config#SetTermCloseOnExit(0)
    call go#util#EchoProgress("term close on exit disabled")
    return
  endif

  call go#config#SetTermCloseOnExit(1)
  call go#util#EchoProgress("term close on exit enabled")
  return
endfunction

function! s:closeterm()
  if !exists('s:lasttermwinid')
    return
  endif

  try
    let l:termwinid = s:lasttermwinid
    unlet s:lasttermwinid
    let l:info = getwininfo(l:termwinid)
    if empty(l:info)
      return
    endif

    let l:info = l:info[0]

    if !get(l:info, 'terminal', 0) is 1
      return
    endif

    if has('nvim')
      if 'goterm' == nvim_buf_get_option(nvim_win_get_buf(l:termwinid), 'filetype')
        call nvim_win_close(l:termwinid, v:true)
      endif
      return
    endif

    if stridx(bufname(winbufnr(l:termwinid)), s:bufnameprefix, 0) == 0
      let l:winid = win_getid()
      call win_gotoid(l:termwinid)
      close!
      call win_gotoid(l:winid)
    endif
  catch
    call go#util#EchoError(printf("vim-go: %s", v:exception))
  endtry
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
