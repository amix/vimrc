if has('nvim') && !exists("g:go_term_mode")
  let g:go_term_mode = 'vsplit'
endif

" s:jobs is a global reference to all jobs started with new()
let s:jobs = {}

" new creates a new terminal with the given command. Mode is set based on the
" global variable g:go_term_mode, which is by default set to :vsplit
function! go#term#new(bang, cmd) abort
  return go#term#newmode(a:bang, a:cmd, g:go_term_mode)
endfunction

" new creates a new terminal with the given command and window mode.
function! go#term#newmode(bang, cmd, mode) abort
  let mode = a:mode
  if empty(mode)
    let mode = g:go_term_mode
  endif

  " execute go build in the files directory
  let l:winnr = winnr()
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

  let job.id = id
  let job.cmd = a:cmd
  startinsert

  " resize new term if needed.
  let height = get(g:, 'go_term_height', winheight(0))
  let width = get(g:, 'go_term_width', winwidth(0))

  " we are careful how to resize. for example it's vsplit we don't change
  " the height. The below command resizes the buffer

  if mode =~ "vertical" || mode =~ "vsplit" || mode =~ "vnew"
    exe 'vertical resize ' . width
  elseif mode =~ "split" || mode =~ "new"
    exe 'resize ' . height
  endif

  " we also need to resize the pty, so there you go...
  call jobresize(id, width, height)

  let s:jobs[id] = job
  stopinsert

  if l:winnr !=# winnr()
    exe l:winnr . "wincmd w"
  endif

  return id
endfunction

function! s:on_stdout(job_id, data, event) dict abort
  if !has_key(s:jobs, a:job_id)
    return
  endif
  let job = s:jobs[a:job_id]

  call extend(job.stdout, a:data)
endfunction

function! s:on_stderr(job_id, data, event) dict abort
  if !has_key(s:jobs, a:job_id)
    return
  endif
  let job = s:jobs[a:job_id]

  call extend(job.stderr, a:data)
endfunction

function! s:on_exit(job_id, exit_status, event) dict abort
  if !has_key(s:jobs, a:job_id)
    return
  endif
  let job = s:jobs[a:job_id]

  let l:listtype = go#list#Type("_term")

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

    call go#list#Populate(l:listtype, errors, job.cmd)
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
