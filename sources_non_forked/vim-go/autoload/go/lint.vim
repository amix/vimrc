" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#lint#Gometa(bang, autosave, ...) abort
  if a:0 == 0
    let goargs = [expand('%:p:h')]
  else
    let goargs = a:000
  endif

  let l:metalinter = go#config#MetalinterCommand()

  if l:metalinter == 'gometalinter' || l:metalinter == 'golangci-lint'
    let cmd = s:metalintercmd(l:metalinter)
    if empty(cmd)
      return
    endif

    " linters
    let linters = a:autosave ? go#config#MetalinterAutosaveEnabled() : go#config#MetalinterEnabled()
    for linter in linters
      let cmd += ["--enable=".linter]
    endfor
  else
    " the user wants something else, let us use it.
    let cmd = split(go#config#MetalinterCommand(), " ")
  endif

  if a:autosave
    " redraw so that any messages that were displayed while writing the file
    " will be cleared
    redraw

    if l:metalinter == "gometalinter"
      " Include only messages for the active buffer for autosave.
      let include = [printf('--include=^%s:.*$', fnamemodify(expand('%:p'), ":."))]
      if go#util#has_job()
        let include = [printf('--include=^%s:.*$', expand('%:p:t'))]
      endif
      let cmd += include
    elseif l:metalinter == "golangci-lint"
      let goargs[0] = expand('%:p:h')
    endif
  endif

  " Call metalinter asynchronously.
  let deadline = go#config#MetalinterDeadline()
  if deadline != ''
    let cmd += ["--deadline=" . deadline]
  endif

  let cmd += goargs

  if l:metalinter == "gometalinter"
    " Gometalinter can output one of the two, so we look for both:
    "   <file>:<line>:<column>:<severity>: <message> (<linter>)
    "   <file>:<line>::<severity>: <message> (<linter>)
    " This can be defined by the following errorformat:
    let errformat = "%f:%l:%c:%t%*[^:]:\ %m,%f:%l::%t%*[^:]:\ %m"
  else
    " Golangci-lint can output the following:
    "   <file>:<line>:<column>: <message> (<linter>)
    " This can be defined by the following errorformat:
    let errformat = "%f:%l:%c:\ %m"
  endif

  if go#util#has_job()
    call s:lint_job({'cmd': cmd, 'statustype': l:metalinter, 'errformat': errformat}, a:bang, a:autosave)
    return
  endif

  let [l:out, l:err] = go#util#Exec(cmd)

  if a:autosave
    let l:listtype = go#list#Type("GoMetaLinterAutoSave")
  else
    let l:listtype = go#list#Type("GoMetaLinter")
  endif

  if l:err == 0
    call go#list#Clean(l:listtype)
    echon "vim-go: " | echohl Function | echon "[metalinter] PASS" | echohl None
  else
    let l:winid = win_getid(winnr())
    " Parse and populate our location list

    let l:messages = split(out, "\n")

    if a:autosave
      call s:metalinterautosavecomplete(fnamemodify(expand('%:p'), ":."), 0, 1, l:messages)
    endif
    call go#list#ParseFormat(l:listtype, errformat, l:messages, 'GoMetaLinter')

    let errors = go#list#Get(l:listtype)
    call go#list#Window(l:listtype, len(errors))

    if a:autosave || a:bang
      call win_gotoid(l:winid)
      return
    endif
    call go#list#JumpToFirst(l:listtype)
  endif
endfunction

" Golint calls 'golint' on the current directory. Any warnings are populated in
" the location list
function! go#lint#Golint(bang, ...) abort
  if a:0 == 0
    let [l:out, l:err] = go#util#Exec([go#config#GolintBin(), expand('%:p:h')])
  else
    let [l:out, l:err] = go#util#Exec([go#config#GolintBin()] + a:000)
  endif

  if empty(l:out)
    call go#util#EchoSuccess('[lint] PASS')
    return
  endif

  let l:winid = win_getid(winnr())
  let l:listtype = go#list#Type("GoLint")
  call go#list#Parse(l:listtype, l:out, "GoLint")
  let l:errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(l:errors))

  if a:bang
    call win_gotoid(l:winid)
    return
  endif

  call go#list#JumpToFirst(l:listtype)
endfunction

" Vet calls 'go vet' on the current directory. Any warnings are populated in
" the location list
function! go#lint#Vet(bang, ...) abort
  call go#cmd#autowrite()

  if go#config#EchoCommandInfo()
    call go#util#EchoProgress('calling vet...')
  endif

  if a:0 == 0
    let [l:out, l:err] = go#util#Exec(['go', 'vet', go#package#ImportPath()])
  else
    let [l:out, l:err] = go#util#ExecInDir(['go', 'tool', 'vet'] + a:000)
  endif

  let l:listtype = go#list#Type("GoVet")
  if l:err != 0
    let l:winid = win_getid(winnr())
    let errorformat = "%-Gexit status %\\d%\\+," . &errorformat
    call go#list#ParseFormat(l:listtype, l:errorformat, out, "GoVet")
    let errors = go#list#Get(l:listtype)
    call go#list#Window(l:listtype, len(errors))
    if !empty(errors) && !a:bang
      call go#list#JumpToFirst(l:listtype)
    else
      call win_gotoid(l:winid)
    endif
  else
    call go#list#Clean(l:listtype)
    call go#util#EchoSuccess('[vet] PASS')
  endif
endfunction

" ErrCheck calls 'errcheck' for the given packages. Any warnings are populated in
" the location list
function! go#lint#Errcheck(bang, ...) abort
  if a:0 == 0
    let l:import_path = go#package#ImportPath()
    if import_path == -1
      call go#util#EchoError('package is not inside GOPATH src')
      return
    endif
  else
    let l:import_path = join(a:000, ' ')
  endif

  call go#util#EchoProgress('[errcheck] analysing ...')
  redraw

  let [l:out, l:err] = go#util#Exec([go#config#ErrcheckBin(), '-abspath', l:import_path])

  let l:listtype = go#list#Type("GoErrCheck")
  if l:err != 0
    let l:winid = win_getid(winnr())
    let errformat = "%f:%l:%c:\ %m, %f:%l:%c\ %#%m"

    " Parse and populate our location list
    call go#list#ParseFormat(l:listtype, errformat, split(out, "\n"), 'Errcheck')

    let l:errors = go#list#Get(l:listtype)
    if empty(l:errors)
      call go#util#EchoError(l:out)
      return
    endif

    if !empty(errors)
      call go#list#Populate(l:listtype, errors, 'Errcheck')
      call go#list#Window(l:listtype, len(errors))
      if !a:bang
        call go#list#JumpToFirst(l:listtype)
      else
        call win_gotoid(l:winid)
      endif
    endif
  else
    call go#list#Clean(l:listtype)
    call go#util#EchoSuccess('[errcheck] PASS')
  endif
endfunction

function! go#lint#ToggleMetaLinterAutoSave() abort
  if go#config#MetalinterAutosave()
    call go#config#SetMetalinterAutosave(0)
    call go#util#EchoProgress("auto metalinter disabled")
    return
  end

  call go#config#SetMetalinterAutosave(1)
  call go#util#EchoProgress("auto metalinter enabled")
endfunction

function! s:lint_job(args, bang, autosave)
  let l:opts = {
        \ 'statustype': a:args.statustype,
        \ 'errorformat': a:args.errformat,
        \ 'for': "GoMetaLinter",
        \ 'bang': a:bang,
        \ }

  if a:autosave
    let l:opts.for = "GoMetaLinterAutoSave"
    let l:opts.complete = funcref('s:metalinterautosavecomplete', [expand('%:p:t')])
  endif

  " autowrite is not enabled for jobs
  call go#cmd#autowrite()

  call go#job#Spawn(a:args.cmd, l:opts)
endfunction

function! s:metalintercmd(metalinter)
  let l:cmd = []
  let bin_path = go#path#CheckBinPath(a:metalinter)
  if !empty(bin_path)
    if a:metalinter == "gometalinter"
      let l:cmd = s:gometalintercmd(bin_path)
    elseif a:metalinter == "golangci-lint"
      let l:cmd = s:golangcilintcmd(bin_path)
    endif
  endif

  return cmd
endfunction

function! s:gometalintercmd(bin_path)
  let cmd = [a:bin_path]
  let cmd += ["--disable-all"]

  " gometalinter has a --tests flag to tell its linters whether to run
  " against tests. While not all of its linters respect this flag, for those
  " that do, it means if we don't pass --tests, the linter won't run against
  " test files. One example of a linter that will not run against tests if
  " we do not specify this flag is errcheck.
  let cmd += ["--tests"]
  return cmd
endfunction

function! s:golangcilintcmd(bin_path)
  let cmd = [a:bin_path]
  let cmd += ["run"]
  let cmd += ["--print-issued-lines=false"]
  let cmd += ['--build-tags', go#config#BuildTags()]
  let cmd += ["--disable-all"]
  " do not use the default exclude patterns, because doing so causes golint
  " problems about missing doc strings to be ignored and other things that
  " golint identifies.
  let cmd += ["--exclude-use-default=false"]

  return cmd
endfunction

function! s:metalinterautosavecomplete(filepath, job, exit_code, messages)
  if len(a:messages) == 0
    return
  endif

  let l:file = expand('%:p:t')
  let l:idx = len(a:messages) - 1
  while l:idx >= 0
    if a:messages[l:idx] !~# '^' . a:filepath . ':'
      call remove(a:messages, l:idx)
    endif
    let l:idx -= 1
  endwhile
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
