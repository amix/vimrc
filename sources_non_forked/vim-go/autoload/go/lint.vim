if !exists("g:go_metalinter_command")
  let g:go_metalinter_command = ""
endif

if !exists("g:go_metalinter_autosave_enabled")
  let g:go_metalinter_autosave_enabled = ['vet', 'golint']
endif

if !exists("g:go_metalinter_enabled")
  let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck']
endif

if !exists("g:go_metalinter_deadline")
  let g:go_metalinter_deadline = "5s"
endif

if !exists("g:go_golint_bin")
  let g:go_golint_bin = "golint"
endif

if !exists("g:go_errcheck_bin")
  let g:go_errcheck_bin = "errcheck"
endif

function! go#lint#Gometa(autosave, ...) abort
  if a:0 == 0
    let goargs = shellescape(expand('%:p:h'))
  else
    let goargs = go#util#Shelljoin(a:000)
  endif

  let bin_path = go#path#CheckBinPath("gometalinter")
  if empty(bin_path)
    return
  endif

  let cmd = [bin_path]
  let cmd += ["--disable-all"]

  if a:autosave || empty(g:go_metalinter_command)
    " linters
    let linters = a:autosave ? g:go_metalinter_autosave_enabled : g:go_metalinter_enabled
    for linter in linters
      let cmd += ["--enable=".linter]
    endfor

    " path
    let cmd += [expand('%:p:h')]
  else
    " the user wants something else, let us use it.
    let cmd += [split(g:go_metalinter_command, " ")]
  endif

  if go#util#has_job() && has('lambda')
    call s:lint_job({'cmd': cmd})
    return
  endif

  " we add deadline only for sync mode
  let cmd += ["--deadline=" . g:go_metalinter_deadline]
  if a:autosave
    " include only messages for the active buffer
    let cmd += ["--include='^" . expand('%:p') . ".*$'"]
  endif


  let meta_command = join(cmd, " ")

  let out = go#tool#ExecuteInDir(meta_command)

  let l:listtype = "quickfix"
  if go#util#ShellError() == 0
    redraw | echo
    call go#list#Clean(l:listtype)
    call go#list#Window(l:listtype)
    echon "vim-go: " | echohl Function | echon "[metalinter] PASS" | echohl None
  else
    " GoMetaLinter can output one of the two, so we look for both:
    "   <file>:<line>:[<column>]: <message> (<linter>)
    "   <file>:<line>:: <message> (<linter>)
    " This can be defined by the following errorformat:
    let errformat = "%f:%l:%c:%t%*[^:]:\ %m,%f:%l::%t%*[^:]:\ %m"

    " Parse and populate our location list
    call go#list#ParseFormat(l:listtype, errformat, split(out, "\n"), 'GoMetaLinter')

    let errors = go#list#Get(l:listtype)
    call go#list#Window(l:listtype, len(errors))

    if !a:autosave
      call go#list#JumpToFirst(l:listtype)
    endif
  endif
endfunction

" Golint calls 'golint' on the current directory. Any warnings are populated in
" the location list
function! go#lint#Golint(...) abort
  let bin_path = go#path#CheckBinPath(g:go_golint_bin) 
  if empty(bin_path) 
    return 
  endif

  if a:0 == 0
    let goargs = shellescape(expand('%'))
  else
    let goargs = go#util#Shelljoin(a:000)
  endif

  let out = go#util#System(bin_path . " " . goargs)
  if empty(out)
    echon "vim-go: " | echohl Function | echon "[lint] PASS" | echohl None
    return
  endif

  let l:listtype = "quickfix"
  call go#list#Parse(l:listtype, out)
  let errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(errors))
  call go#list#JumpToFirst(l:listtype)
endfunction

" Vet calls 'go vet' on the current directory. Any warnings are populated in
" the location list
function! go#lint#Vet(bang, ...) abort
  call go#cmd#autowrite()
  echon "vim-go: " | echohl Identifier | echon "calling vet..." | echohl None
  if a:0 == 0
    let out = go#tool#ExecuteInDir('go vet')
  else
    let out = go#tool#ExecuteInDir('go tool vet ' . go#util#Shelljoin(a:000))
  endif

  let l:listtype = "quickfix"
  if go#util#ShellError() != 0
    let errors = go#tool#ParseErrors(split(out, '\n'))
    call go#list#Populate(l:listtype, errors, 'Vet')
    call go#list#Window(l:listtype, len(errors))
    if !empty(errors) && !a:bang
      call go#list#JumpToFirst(l:listtype)
    endif
    echon "vim-go: " | echohl ErrorMsg | echon "[vet] FAIL" | echohl None
  else
    call go#list#Clean(l:listtype)
    call go#list#Window(l:listtype)
    redraw | echon "vim-go: " | echohl Function | echon "[vet] PASS" | echohl None
  endif
endfunction

" ErrCheck calls 'errcheck' for the given packages. Any warnings are populated in
" the location list
function! go#lint#Errcheck(...) abort
  if a:0 == 0
    let goargs = go#package#ImportPath(expand('%:p:h'))
    if goargs == -1
      echohl Error | echomsg "vim-go: package is not inside GOPATH src" | echohl None
      return
    endif
  else
    let goargs = go#util#Shelljoin(a:000)
  endif

  let bin_path = go#path#CheckBinPath(g:go_errcheck_bin)
  if empty(bin_path)
    return
  endif

  echon "vim-go: " | echohl Identifier | echon "errcheck analysing ..." | echohl None
  redraw

  let command = bin_path . ' -abspath ' . goargs
  let out = go#tool#ExecuteInDir(command)

  let l:listtype = "quickfix"
  if go#util#ShellError() != 0
    let errformat = "%f:%l:%c:\ %m, %f:%l:%c\ %#%m"

    " Parse and populate our location list
    call go#list#ParseFormat(l:listtype, errformat, split(out, "\n"), 'Errcheck')

    let errors = go#list#Get(l:listtype)

    if empty(errors)
      echohl Error | echomsg "GoErrCheck returned error" | echohl None
      echo out
      return
    endif

    if !empty(errors)
      call go#list#Populate(l:listtype, errors, 'Errcheck')
      call go#list#Window(l:listtype, len(errors))
      if !empty(errors)
        call go#list#JumpToFirst(l:listtype)
      endif
    endif
  else
    call go#list#Clean(l:listtype)
    call go#list#Window(l:listtype)
    echon "vim-go: " | echohl Function | echon "[errcheck] PASS" | echohl None
  endif

endfunction

function! go#lint#ToggleMetaLinterAutoSave() abort
  if get(g:, "go_metalinter_autosave", 0)
    let g:go_metalinter_autosave = 0
    call go#util#EchoProgress("auto metalinter disabled")
    return
  end

  let g:go_metalinter_autosave = 1
  call go#util#EchoProgress("auto metalinter enabled")
endfunction

function s:lint_job(args)
  let status_dir = expand('%:p:h')
  let started_at = reltime()

  call go#statusline#Update(status_dir, {
        \ 'desc': "current status",
        \ 'type': "gometalinter",
        \ 'state': "analysing",
        \})

  " autowrite is not enabled for jobs
  call go#cmd#autowrite()

  let l:listtype = go#list#Type("quickfix")
  let l:errformat = '%f:%l:%c:%t%*[^:]:\ %m,%f:%l::%t%*[^:]:\ %m'

  function! s:callback(chan, msg) closure
    let old_errorformat = &errorformat
    let &errorformat = l:errformat
    caddexpr a:msg
    let &errorformat = old_errorformat

    " TODO(arslan): cursor still jumps to first error even If I don't want
    " it. Seems like there is a regression somewhere, but not sure where.
    copen
  endfunction

  function! s:close_cb(chan) closure
    let l:job = ch_getjob(a:chan)
    let l:status = job_status(l:job)

    let exitval = 1
    if l:status == "dead"
      let l:info = job_info(l:job)
      let exitval = l:info.exitval
    endif

    let status = {
          \ 'desc': 'last status',
          \ 'type': "gometaliner",
          \ 'state': "finished",
          \ }

    if exitval
      let status.state = "failed"
    endif

    let elapsed_time = reltimestr(reltime(started_at))
    " strip whitespace
    let elapsed_time = substitute(elapsed_time, '^\s*\(.\{-}\)\s*$', '\1', '')
    let status.state .= printf(" (%ss)", elapsed_time)

    call go#statusline#Update(status_dir, status)

    let errors = go#list#Get(l:listtype)
    if empty(errors) 
      call go#list#Window(l:listtype, len(errors))
    elseif has("patch-7.4.2200")
      if l:listtype == 'quickfix'
        call setqflist([], 'a', {'title': 'GoMetaLinter'})
      else
        call setloclist(0, [], 'a', {'title': 'GoMetaLinter'})
      endif
    endif

    if get(g:, 'go_echo_command_info', 1)
      call go#util#EchoSuccess("linting finished")
    endif
  endfunction

  let start_options = {
        \ 'callback': function("s:callback"),
        \ 'close_cb': function("s:close_cb"),
        \ }

  call job_start(a:args.cmd, start_options)

  call go#list#Clean(l:listtype)

  if get(g:, 'go_echo_command_info', 1)
    call go#util#EchoProgress("linting started ...")
  endif
endfunction

" vim: sw=2 ts=2 et
