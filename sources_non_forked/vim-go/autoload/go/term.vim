" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

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

  let l:state = {
        \ 'cmd': a:cmd,
        \ 'bang' : a:bang,
        \ 'winid': win_getid(winnr()),
        \ 'stdout': [],
        \ 'stdout_buf': '',
        \ 'errorformat': a:errorformat,
      \ }

  " execute go build in the files directory
  let l:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let l:dir = getcwd()

  execute l:cd . fnameescape(expand("%:p:h"))

  execute l:mode . ' __go_term__'

  setlocal filetype=goterm
  setlocal bufhidden=delete
  setlocal winfixheight
  setlocal noswapfile
  setlocal nobuflisted

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

  execute l:cd . fnameescape(l:dir)

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

  call win_gotoid(l:state.winid)

  return l:state.id
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

function! s:on_exit(job_id, exit_status, event) dict abort
  let l:winid = win_getid(winnr())
  call win_gotoid(self.winid)
  let l:listtype = go#list#Type("_term")

  if a:exit_status == 0
    call go#list#Clean(l:listtype)
    call win_gotoid(l:winid)
    return
  endif

  call win_gotoid(self.winid)

  let l:title = self.cmd
  if type(l:title) == v:t_list
    let l:title = join(self.cmd)
  endif

  let l:i = 0
  while l:i < len(self.stdout)
    let self.stdout[l:i] = substitute(self.stdout[l:i], "\r$", '', 'g')
    let l:i += 1
  endwhile

  call go#list#ParseFormat(l:listtype, self.errorformat, self.stdout, l:title)
  let l:errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(l:errors))

  if empty(l:errors)
    call go#util#EchoError( '[' . l:title . '] ' . "FAIL")
    call win_gotoid(l:winid)
    return
  endif

  " close terminal; we don't need it anymore
  call win_gotoid(self.termwinid)
  close!

  if self.bang
    call win_gotoid(l:winid)
    return
  endif

  call win_gotoid(self.winid)
  call go#list#JumpToFirst(l:listtype)
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
