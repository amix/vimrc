" s:jobs is a global reference to all jobs started with Spawn() or with the
" internal function s:spawn
let s:jobs = {}

" s:handlers is a global event handlers for all jobs started with Spawn() or
" with the internal function s:spawn
let s:handlers = {}

" Spawn is a wrapper around s:spawn. It can be executed by other files and
" scripts if needed. Desc defines the description for printing the status
" during the job execution (useful for statusline integration).
function! go#jobcontrol#Spawn(bang, desc, args) abort
  " autowrite is not enabled for jobs
  call go#cmd#autowrite()

  let job = s:spawn(a:bang, a:desc, a:args)
  return job.id
endfunction

" AddHandler adds a on_exit callback handler and returns the id.
function! go#jobcontrol#AddHandler(handler) abort
  let i = len(s:handlers)
  while has_key(s:handlers, string(i))
    let i += 1
    break
  endwhile
  let s:handlers[string(i)] = a:handler
  return string(i)
endfunction

" RemoveHandler removes a callback handler by id.
function! go#jobcontrol#RemoveHandler(id) abort
  unlet s:handlers[a:id]
endfunction

" spawn spawns a go subcommand with the name and arguments with jobstart. Once
" a job is started a reference will be stored inside s:jobs. spawn changes the
" GOPATH when g:go_autodetect_gopath is enabled. The job is started inside the
" current files folder.
function! s:spawn(bang, desc, args) abort
  let job = {
        \ 'desc': a:desc,
        \ 'bang': a:bang,
        \ 'winnr': winnr(),
        \ 'importpath': go#package#ImportPath(expand('%:p:h')),
        \ 'state': "RUNNING",
        \ 'stderr' : [],
        \ 'stdout' : [],
        \ 'on_stdout': function('s:on_stdout'),
        \ 'on_stderr': function('s:on_stderr'),
        \ 'on_exit' : function('s:on_exit'),
        \ }

  " modify GOPATH if needed
  let old_gopath = $GOPATH
  let $GOPATH = go#path#Detect()

  " execute go build in the files directory
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '

  " cleanup previous jobs for this file
  for jb in values(s:jobs)
    if jb.importpath == job.importpath
      unlet s:jobs[jb.id]
    endif
  endfor

  let dir = getcwd()
  let jobdir = fnameescape(expand("%:p:h"))
  execute cd . jobdir

  " append the subcommand, such as 'build'
  let argv = ['go'] + a:args

  " run, forrest, run!
  let id = jobstart(argv, job)
  let job.id = id
  let job.dir = jobdir
  let s:jobs[id] = job

  execute cd . fnameescape(dir)

  " restore back GOPATH
  let $GOPATH = old_gopath

  return job
endfunction

" on_exit is the exit handler for jobstart(). It handles cleaning up the job
" references and also displaying errors in the quickfix window collected by
" on_stderr handler. If there are no errors and a quickfix window is open,
" it'll be closed.
function! s:on_exit(job_id, exit_status, event) dict abort
  let std_combined = self.stderr + self.stdout

  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  execute cd self.dir

  call s:callback_handlers_on_exit(s:jobs[a:job_id], a:exit_status, std_combined)

  let l:listtype = go#list#Type("quickfix")
  if a:exit_status == 0
    call go#list#Clean(l:listtype)
    call go#list#Window(l:listtype)

    let self.state = "SUCCESS"
    call go#util#EchoSuccess("SUCCESS")

    execute cd . fnameescape(dir)
    return
  endif

  let self.state = "FAILED"
  call go#util#EchoError("FAILED")

  let errors = go#tool#ParseErrors(std_combined)
  let errors = go#tool#FilterValids(errors)

  execute cd . fnameescape(dir)

  if !len(errors)
    " failed to parse errors, output the original content
    call go#util#EchoError(std_combined[0])
    return
  endif

  " if we are still in the same windows show the list
  if self.winnr == winnr()
    call go#list#Populate(l:listtype, errors, self.desc)
    call go#list#Window(l:listtype, len(errors))
    if !empty(errors) && !self.bang
      call go#list#JumpToFirst(l:listtype)
    endif
  endif
endfunction

" callback_handlers_on_exit runs all handlers for job on exit event.
function! s:callback_handlers_on_exit(job, exit_status, data) abort
  if empty(s:handlers)
    return
  endif

  for s:handler in values(s:handlers)
    call s:handler(a:job, a:exit_status, a:data)
  endfor
endfunction

" on_stdout is the stdout handler for jobstart(). It collects the output of
" stderr and stores them to the jobs internal stdout list.
function! s:on_stdout(job_id, data) dict abort
  call extend(self.stdout, a:data)
endfunction

" on_stderr is the stderr handler for jobstart(). It collects the output of
" stderr and stores them to the jobs internal stderr list.
function! s:on_stderr(job_id, data) dict abort
  call extend(self.stderr, a:data)
endfunction

" vim: sw=2 ts=2 et
