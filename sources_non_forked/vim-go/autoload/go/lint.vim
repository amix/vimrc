function! go#lint#Gometa(autosave, ...) abort
  if a:0 == 0
    let goargs = [expand('%:p:h')]
  else
    let goargs = a:000
  endif

  let bin_path = go#path#CheckBinPath("gometalinter")
  if empty(bin_path)
    return
  endif

  let cmd = [bin_path]
  let cmd += ["--disable-all"]

  if a:autosave || empty(go#config#MetalinterCommand())
    " linters
    let linters = a:autosave ? go#config#MetalinterAutosaveEnabled() : go#config#MetalinterEnabled()
    for linter in linters
      let cmd += ["--enable=".linter]
    endfor

    for linter in go#config#MetalinterDisabled()
      let cmd += ["--disable=".linter]
    endfor

    " gometalinter has a --tests flag to tell its linters whether to run
    " against tests. While not all of its linters respect this flag, for those
    " that do, it means if we don't pass --tests, the linter won't run against
    " test files. One example of a linter that will not run against tests if
    " we do not specify this flag is errcheck.
    let cmd += ["--tests"]
  else
    " the user wants something else, let us use it.
    let cmd += split(go#config#MetalinterCommand(), " ")
  endif

  if a:autosave
    " redraw so that any messages that were displayed while writing the file
    " will be cleared
    redraw

    " Include only messages for the active buffer for autosave.
    let include = [printf('--include=^%s:.*$', fnamemodify(expand('%:p'), ":."))]
    if go#util#has_job()
      let include = [printf('--include=^%s:.*$', expand('%:p:t'))]
    endif
    let cmd += include
  endif

  " Call gometalinter asynchronously.
  let deadline = go#config#MetalinterDeadline()
  if deadline != ''
    let cmd += ["--deadline=" . deadline]
  endif

  let cmd += goargs

  if go#util#has_job()
    call s:lint_job({'cmd': cmd}, a:autosave)
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
  if a:0 == 0
    let [l:out, l:err] = go#util#Exec([go#config#GolintBin(), go#package#ImportPath()])
  else
    let [l:out, l:err] = go#util#Exec([go#config#GolintBin()] + a:000)
  endif

  if empty(l:out)
    call go#util#EchoSuccess('[lint] PASS')
    return
  endif

  let l:listtype = go#list#Type("GoLint")
  call go#list#Parse(l:listtype, l:out, "GoLint")
  let l:errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(l:errors))
  call go#list#JumpToFirst(l:listtype)
endfunction

" Vet calls 'go vet' on the current directory. Any warnings are populated in
" the location list
function! go#lint#Vet(bang, ...) abort
  call go#cmd#autowrite()

  call go#util#EchoProgress('calling vet...')

  if a:0 == 0
    let [l:out, l:err] = go#util#Exec(['go', 'vet', go#package#ImportPath()])
  else
    let [l:out, l:err] = go#util#Exec(['go', 'tool', 'vet'] + a:000)
  endif

  let l:listtype = go#list#Type("GoVet")
  if l:err != 0
    let errorformat = "%-Gexit status %\\d%\\+," . &errorformat
    call go#list#ParseFormat(l:listtype, l:errorformat, out, "GoVet")
    let errors = go#list#Get(l:listtype)
    call go#list#Window(l:listtype, len(errors))
    if !empty(errors) && !a:bang
      call go#list#JumpToFirst(l:listtype)
    endif
    call go#util#EchoError('[vet] FAIL')
  else
    call go#list#Clean(l:listtype)
    call go#util#EchoSuccess('[vet] PASS')
  endif
endfunction

" ErrCheck calls 'errcheck' for the given packages. Any warnings are populated in
" the location list
function! go#lint#Errcheck(...) abort
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
      if !empty(errors)
        call go#list#JumpToFirst(l:listtype)
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

function! s:lint_job(args, autosave)
  let l:opts = {
        \ 'statustype': "gometalinter",
        \ 'errorformat': '%f:%l:%c:%t%*[^:]:\ %m,%f:%l::%t%*[^:]:\ %m',
        \ 'for': "GoMetaLinter",
        \ }

  if a:autosave
    let l:opts.for = "GoMetaLinterAutoSave"
  endif

  " autowrite is not enabled for jobs
  call go#cmd#autowrite()

  call go#job#Spawn(a:args.cmd, l:opts)
endfunction

" vim: sw=2 ts=2 et
