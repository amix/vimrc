" s:jobs is a global reference to all jobs started with Spawn() or with the
" internal function s:spawn
let s:jobs = {}

" Spawn is a wrapper around s:spawn. It can be executed by other files and
" scripts if needed. Desc defines the description for printing the status
" during the job execution (useful for statusline integration).
function! go#jobcontrol#Spawn(bang, desc, args)
  " autowrite is not enabled for jobs
  call go#cmd#autowrite()

  let job = s:spawn(a:bang, a:desc, a:args)
  return job.id
endfunction

" Statusline returns the current status of the job
function! go#jobcontrol#Statusline() abort
  if empty(s:jobs)
    return ''
  endif

  let import_path =  go#package#ImportPath(expand('%:p:h'))

  for job in values(s:jobs)
    if job.importpath != import_path
      continue
    endif

    if job.state == "SUCCESS"
      return ''
    endif

    return printf("%s ... [%s]", job.desc, job.state)
  endfor

  return ''
endfunction

" spawn spawns a go subcommand with the name and arguments with jobstart. Once
" a job is started a reference will be stored inside s:jobs. spawn changes the
" GOPATH when g:go_autodetect_gopath is enabled. The job is started inside the
" current files folder.
function! s:spawn(bang, desc, args)
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
function! s:on_exit(job_id, exit_status)
  let std_combined = self.stderr + self.stdout
  if a:exit_status == 0
    call go#list#Clean()
    call go#list#Window()

    let self.state = "SUCCESS"
    return
  endif

  let self.state = "FAILED"

  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  try
    execute cd self.dir
    let errors = go#tool#ParseErrors(std_combined)
    let errors = go#tool#FilterValids(errors)
  finally
    execute cd . fnameescape(dir)
  endtry

  if !len(errors)
    " failed to parse errors, output the original content
    call go#util#EchoError(std_combined[0])
    return
  endif

  " if we are still in the same windows show the list
  if self.winnr == winnr()
    call go#list#Populate(errors)
    call go#list#Window(len(errors))
    if !empty(errors) && !self.bang
      call go#list#JumpToFirst()
    endif
  endif
endfunction

" on_stdout is the stdout handler for jobstart(). It collects the output of
" stderr and stores them to the jobs internal stdout list. 
function! s:on_stdout(job_id, data)
  call extend(self.stdout, a:data)
endfunction

" on_stderr is the stderr handler for jobstart(). It collects the output of
" stderr and stores them to the jobs internal stderr list.
function! s:on_stderr(job_id, data)
  call extend(self.stderr, a:data)
endfunction

" abort_all aborts all current jobs created with s:spawn()
function! s:abort_all()
  if empty(s:jobs)
    return
  endif

  for id in keys(s:jobs)
    if id > 0
      silent! call jobstop(id)
    endif
  endfor

  let s:jobs = {}
endfunction

" abort aborts the job with the given name, where name is the first argument
" passed to s:spawn()
function! s:abort(path)
  if empty(s:jobs)
    return
  endif

  for job in values(s:jobs)
    if job.importpath == path && job.id > 0
      silent! call jobstop(job.id)
      unlet s:jobs['job.id']
    endif
  endfor
endfunction

" vim:ts=2:sw=2:et
