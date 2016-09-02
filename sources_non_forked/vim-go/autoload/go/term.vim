if has('nvim') && !exists("g:go_term_mode")
  let g:go_term_mode = 'vsplit'
endif

" s:jobs is a global reference to all jobs started with new()
let s:jobs = {}

" new creates a new terminal with the given command. Mode is set based on the
" global variable g:go_term_mode, which is by default set to :vsplit
function! go#term#new(bang, cmd)
  return go#term#newmode(a:bang, a:cmd, g:go_term_mode)
endfunction

" new creates a new terminal with the given command and window mode.
function! go#term#newmode(bang, cmd, mode)
  let mode = a:mode
  if empty(mode)
    let mode = g:go_term_mode
  endif

  " modify GOPATH if needed
  let old_gopath = $GOPATH
  let $GOPATH = go#path#Detect()

  " execute go build in the files directory
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()

  execute cd . fnameescape(expand("%:p:h"))

  execute mode.' __go_term__'

  setlocal filetype=goterm
  setlocal bufhidden=delete
  setlocal winfixheight
  setlocal noswapfile
  setlocal nobuflisted

  let job = { 
        \ 'stderr' : [],
        \ 'stdout' : [],
        \ 'bang' : a:bang,
        \ 'on_stdout': function('s:on_stdout'),
        \ 'on_stderr': function('s:on_stderr'),
        \ 'on_exit' : function('s:on_exit'),
        \ }

  let id = termopen(a:cmd, job)

  execute cd . fnameescape(dir)

  " restore back GOPATH
  let $GOPATH = old_gopath

  let job.id = id
  startinsert

  " resize new term if needed.
  let height = get(g:, 'go_term_height', winheight(0))
  let width = get(g:, 'go_term_width', winwidth(0))

  " we are careful how to resize. for example it's vertical we don't change
  " the height. The below command resizes the buffer
  if a:mode == "split"
    exe 'resize ' . height
  elseif a:mode == "vertical"
    exe 'vertical resize ' . width
  endif

  " we also need to resize the pty, so there you go...
  call jobresize(id, width, height)

  let s:jobs[id] = job
  return id
endfunction

function! s:on_stdout(job_id, data)
  if !has_key(s:jobs, a:job_id)
    return
  endif
  let job = s:jobs[a:job_id]

  call extend(job.stdout, a:data)
endfunction

function! s:on_stderr(job_id, data)
  if !has_key(s:jobs, a:job_id)
    return
  endif
  let job = s:jobs[a:job_id]

  call extend(job.stderr, a:data)
endfunction

function! s:on_exit(job_id, exit_status)
  if !has_key(s:jobs, a:job_id)
    return
  endif
  let job = s:jobs[a:job_id]

  let l:listtype = "locationlist"

  " usually there is always output so never branch into this clause
  if empty(job.stdout)
    call go#list#Clean(l:listtype)
    call go#list#Window(l:listtype)
    unlet s:jobs[a:job_id]
    return
  endif

  let errors = go#tool#ParseErrors(job.stdout)
  let errors = go#tool#FilterValids(errors)

  if !empty(errors)
    " close terminal we don't need it anymore
    close 

    call go#list#Populate(l:listtype, errors)
    call go#list#Window(l:listtype, len(errors))
    if !self.bang
      call go#list#JumpToFirst(l:listtype)
    endif
    unlet s:jobs[a:job_id]
    return
  endif

    " tests are passing clean the list and close the list. But we only can
    " close them from a normal view, so jump back, close the list and then
    " again jump back to the terminal
    wincmd p
    call go#list#Clean(l:listtype)
    call go#list#Window(l:listtype)
    wincmd p

    unlet s:jobs[a:job_id]
endfunction

" vim: sw=2 ts=2 et
