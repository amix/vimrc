" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#lint#Gometa(bang, autosave, ...) abort
  let l:metalinter = go#config#MetalinterCommand()

  if a:0 == 0
    let l:goargs = [expand('%:p:h')]
    if l:metalinter == 'gopls' || l:metalinter == 'staticcheck'
      let l:pkg = go#package#ImportPath()
      if l:pkg == -1
        call go#util#EchoError('could not determine package name')
        return
      endif

      let l:goargs = [l:pkg]
    endif
  else
    let l:goargs = a:000
  endif

  let l:cmd = []
  let l:linters = a:autosave ? go#config#MetalinterAutosaveEnabled() : go#config#MetalinterEnabled()
  if l:metalinter == 'golangci-lint'
    let l:cmd = s:metalintercmd(l:metalinter, len(linters) != 0)
    if empty(l:cmd)
      return
    endif

    " add linters to cmd
    for l:linter in l:linters
      let l:cmd += ["--enable=".l:linter]
    endfor
  elseif l:metalinter == 'staticcheck'
    let l:cmd = s:metalintercmd(l:metalinter, 0)

    if len(l:linters) > 0
      let l:cmd += [printf('-checks=%s', join(l:linters, ',' ))]
    endif
  elseif l:metalinter != 'gopls'
    " the user wants something else, let us use it.
    let l:cmd = split(go#config#MetalinterCommand(), " ")
  endif

  if a:autosave
    " redraw so that any messages that were displayed while writing the file
    " will be cleared
    redraw

    let l:goargs[0] = expand('%:p')
    if l:metalinter == 'staticcheck' || l:metalinter == "golangci-lint"
      let l:goargs[0] = expand('%:p:h')
    endif
  endif

  " Call metalinter asynchronously.

  if l:metalinter == 'golangci-lint'
    let l:deadline = go#config#MetalinterDeadline()
    if l:deadline != ''
      let l:cmd += ["--deadline=" . l:deadline]
    endif
  endif

  let l:cmd += l:goargs

  let l:errformat = s:errorformat(l:metalinter)

  if l:metalinter == 'gopls'
    if a:autosave
      let l:messages = go#lsp#AnalyzeFile(expand('%:p'))
    else
      let l:import_paths = l:goargs
      let l:messages = call('go#lsp#Diagnostics', l:import_paths)
    endif

    let l:err = len(l:messages)
  else
    if go#util#has_job()
      if a:autosave
        let l:for = 'GoMetaLinterAutoSave'
      else
        let l:for = 'GoMetaLinter'
      endif

      call s:lint_job(l:metalinter, {'cmd': l:cmd, 'statustype': l:metalinter, 'errformat': l:errformat, 'for': l:for}, a:bang, a:autosave)
      return
    endif

    let [l:out, l:err] = go#util#Exec(l:cmd)
    let l:messages = split(out, "\n")
  endif

  if a:autosave
    let l:listtype = go#list#Type('GoMetaLinterAutoSave')
    let l:for = 'GoMetaLinterAutoSave'
  else
    let l:listtype = go#list#Type('GoMetaLinter')
    let l:for = 'GoMetaLinter'
  endif

  if l:err == 0
    if !s:preserveerrors(a:autosave, l:listtype)
      call go#list#Clean(l:listtype)
    endif
    call go#util#EchoSuccess('[metalinter] PASS')
  else
    let l:winid = win_getid(winnr())
    " Parse and populate our location list

    if a:autosave
      call s:metalinterautosavecomplete(l:metalinter, fnamemodify(expand('%:p'), ':.'), 0, 1, l:messages)
    endif
    call go#list#ParseFormat(l:listtype, l:errformat, l:messages, l:for, s:preserveerrors(a:autosave, l:listtype))

    let errors = go#list#Get(l:listtype)
    call go#list#Window(l:listtype, len(errors))

    if a:autosave || a:bang
      call win_gotoid(l:winid)
    else
      call go#list#JumpToFirst(l:listtype)
    endif
  endif
endfunction

function! go#lint#Diagnostics(bang, ...) abort
  if a:0 == 0
    let l:pkg = go#package#ImportPath()
    if l:pkg == -1
      call go#util#EchoError('could not determine package name')
      return
    endif

    let l:import_paths = [l:pkg]
  else
    let l:import_paths = call('go#util#ExpandPattern', a:000)
  endif

  let l:errformat = s:errorformat('gopls')

  let l:messages = call('go#lsp#Diagnostics', l:import_paths)

  let l:listtype = go#list#Type("GoDiagnostics")

  " Parse and populate the quickfix list
  let l:winid = win_getid(winnr())
  call go#list#ParseFormat(l:listtype, l:errformat, l:messages, 'GoDiagnostics', 0)

  let l:errors = go#list#Get(l:listtype)

  if len(l:errors) == 0
    call go#list#Clean(l:listtype)
    call go#util#EchoSuccess('[diagnostics] PASS')
  else
    call go#list#Window(l:listtype, len(errors))

    if a:bang
      call win_gotoid(l:winid)
    else
      call go#list#JumpToFirst(l:listtype)
    endif
  endif
endfunction

" Golint calls 'golint' on the current directory. Any warnings are populated in
" the location list
function! go#lint#Golint(bang, ...) abort
  call go#cmd#autowrite()

  let l:type = 'lint'
  let l:status = {
        \ 'desc': 'current status',
        \ 'type': l:type,
        \ 'state': "started",
        \ }
  if go#config#EchoCommandInfo()
    call go#util#EchoProgress(printf('[%s] analyzing...', l:type))
  endif
  call go#statusline#Update(expand('%:p:h'), l:status)

  if a:0 == 0
    let [l:out, l:err] = go#util#Exec([go#config#GolintBin(), expand('%:p:h')])
  else
    let [l:out, l:err] = go#util#Exec([go#config#GolintBin()] + a:000)
  endif

  let l:status.state = 'success'
  let l:state = 'PASS'
  let l:listtype = go#list#Type("GoLint")
  if !empty(l:out)
    let l:status.state = 'failed'
    let l:state = 'FAIL'

    let l:winid = win_getid(winnr())
    call go#list#Parse(l:listtype, l:out, "GoLint", 0)
    let l:errors = go#list#Get(l:listtype)
    call go#list#Window(l:listtype, len(l:errors))

    if a:bang
      call win_gotoid(l:winid)
    else
      call go#list#JumpToFirst(l:listtype)
    endif
    if go#config#EchoCommandInfo()
      call go#util#EchoError(printf('[%s] %s', l:type, l:state))
    endif
  else
    call go#list#Clean(l:listtype)
    if go#config#EchoCommandInfo()
      call go#util#EchoSuccess(printf('[%s] %s', l:type, l:state))
    endif
  endif
  call go#statusline#Update(expand('%:p:h'), l:status)
endfunction

" Vet calls 'go vet' on the current buffer's directory. Any warnings are
" populated in the location list
function! go#lint#Vet(bang, ...) abort
  call go#cmd#autowrite()

  let l:cmd = ['go', 'vet']

  let buildtags = go#config#BuildTags()
  if buildtags isnot ''
    let l:cmd += ['-tags', buildtags]
  endif

  if a:0 == 0
    let l:import_path = go#package#ImportPath()
    if l:import_path == -1
      call go#util#EchoError('could not determine package')
      return
    endif
    let l:cmd = add(l:cmd, l:import_path)
  else
    let l:cmd = extend(l:cmd, a:000)
  endif

  let l:type = 'go vet'
  if go#config#EchoCommandInfo()
    call go#util#EchoProgress(printf('[%s] analyzing...', l:type))
  endif
  let l:status = {
        \ 'desc': 'current status',
        \ 'type': l:type,
        \ 'state': "started",
        \ }
  call go#statusline#Update(expand('%:p:h'), l:status)

  let [l:out, l:err] = go#util#ExecInDir(l:cmd)

  let l:status.state = 'success'
  let l:state = 'PASS'

  let l:listtype = go#list#Type("GoVet")
  if l:err != 0
    let l:status.state = 'failed'
    let l:state = 'FAIL'

    let l:winid = win_getid(winnr())
    let l:errorformat = "%-Gexit status %\\d%\\+," . &errorformat
    let l:dir = go#util#Chdir(expand('%:p:h'))
    try
      call go#list#ParseFormat(l:listtype, l:errorformat, out, "GoVet", 0)
    finally
      call go#util#Chdir(l:dir)
    endtry
    let l:errors = go#list#Get(l:listtype)

    if empty(l:errors)
      call go#util#EchoError(l:out)
      return
    endif

    call go#list#Window(l:listtype, len(l:errors))
    if !empty(l:errors) && !a:bang
      call go#list#JumpToFirst(l:listtype)
    else
      call win_gotoid(l:winid)
    endif

    if go#config#EchoCommandInfo()
      call go#util#EchoError(printf('[%s] %s', l:type, l:state))
    endif
  else
    call go#list#Clean(l:listtype)
    if go#config#EchoCommandInfo()
      call go#util#EchoSuccess(printf('[%s] %s', l:type, l:state))
    endif
  endif
  call go#statusline#Update(expand('%:p:h'), l:status)
endfunction

" ErrCheck calls 'errcheck' for the given packages. Any warnings are populated in
" the location list
function! go#lint#Errcheck(bang, ...) abort
  call go#cmd#autowrite()

  let l:cmd = [go#config#ErrcheckBin(), '-abspath']

  let buildtags = go#config#BuildTags()
  if buildtags isnot ''
    let l:cmd += ['-tags', buildtags]
  endif

  if a:0 == 0
    let l:import_path = go#package#ImportPath()
    if l:import_path == -1
      call go#util#EchoError('could not determine package')
      return
    endif
    let l:cmd = add(l:cmd, l:import_path)
  else
    let l:cmd = extend(l:cmd, a:000)
  endif

  let l:type = 'errcheck'
  if go#config#EchoCommandInfo()
    call go#util#EchoProgress(printf('[%s] analyzing...', l:type))
  endif
  let l:status = {
        \ 'desc': 'current status',
        \ 'type': l:type,
        \ 'state': "started",
        \ }
  redraw

  call go#statusline#Update(expand('%:p:h'), l:status)

  let [l:out, l:err] = go#util#ExecInDir(l:cmd)

  let l:status.state = 'success'
  let l:state = 'PASS'

  let l:listtype = go#list#Type("GoErrCheck")
  if l:err != 0
    let l:status.state = 'failed'
    let l:state = 'FAIL'

    let l:winid = win_getid(winnr())

    if l:err == 1
      let l:errformat = "%f:%l:%c:\ %m,%f:%l:%c\ %#%m"
      " Parse and populate our location list
      call go#list#ParseFormat(l:listtype, l:errformat, split(out, "\n"), 'Errcheck', 0)
    endif

    let l:errors = go#list#Get(l:listtype)
    if empty(l:errors)
      call go#util#EchoError(l:out)
      return
    endif

    if !empty(errors)
      call go#list#Populate(l:listtype, l:errors, 'Errcheck')
      call go#list#Window(l:listtype, len(l:errors))
      if !a:bang
        call go#list#JumpToFirst(l:listtype)
      else
        call win_gotoid(l:winid)
      endif
    endif
    if go#config#EchoCommandInfo()
      call go#util#EchoError(printf('[%s] %s', l:type, l:state))
    endif
  else
    call go#list#Clean(l:listtype)
    if go#config#EchoCommandInfo()
      call go#util#EchoSuccess(printf('[%s] %s', l:type, l:state))
    endif
  endif
  call go#statusline#Update(expand('%:p:h'), l:status)
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

function! s:lint_job(metalinter, args, bang, autosave)
  let l:opts = {
        \ 'statustype': a:args.statustype,
        \ 'errorformat': a:args.errformat,
        \ 'for': 'GoMetaLinter',
        \ 'bang': a:bang,
      \ }

  if a:autosave
    let l:opts.for = 'GoMetaLinterAutoSave'
    " s:metalinterautosavecomplete is needed for staticcheck and golangci-lint
    let l:opts.complete = funcref('s:metalinterautosavecomplete', [a:metalinter, expand('%:p:t')])
    let l:opts.preserveerrors = funcref('s:preserveerrors', [a:autosave])
  endif

  " autowrite is not enabled for jobs
  call go#cmd#autowrite()

  call go#job#Spawn(a:args.cmd, l:opts)
endfunction

function! s:metalintercmd(metalinter, haslinter)
  let l:cmd = []
  let l:bin_path = go#path#CheckBinPath(a:metalinter)
  if !empty(l:bin_path)
    if a:metalinter == "golangci-lint"
      let l:cmd = s:golangcilintcmd(l:bin_path, a:haslinter)
    elseif a:metalinter == 'staticcheck'
      let l:cmd = [l:bin_path]
    endif
  endif

  return l:cmd
endfunction

function! s:golangcilintcmd(bin_path, haslinter)
  let l:cmd = [a:bin_path]
  let l:cmd += ["run"]
  let l:cmd += ["--print-issued-lines=false"]
  let l:cmd += ['--build-tags', go#config#BuildTags()]
  " do not use the default exclude patterns, because doing so causes golint
  " problems about missing doc strings to be ignored and other things that
  " golint identifies.
  let l:cmd += ["--exclude-use-default=false"]

  if a:haslinter
    let l:cmd += ["--disable-all"]
  endif

  return l:cmd
endfunction

function! s:metalinterautosavecomplete(metalinter, filepath, job, exit_code, messages)
  if !(a:metalinter == 'golangci-lint' || a:metalinter == 'staticcheck')
    return
  endif

  if len(a:messages) == 0
    return
  endif

  let l:idx = 0
  for l:item in a:messages
    " leave in any messages that report errors about a:filepath or that report
    " more general problems that prevent golangci-lint from linting
    " a:filepath.
    if l:item =~# '^' . a:filepath . ':' || (a:metalinter == 'golangci-lint' && l:item =~# '^level=')
      let l:idx += 1
      continue
    endif
    call remove(a:messages, l:idx)
  endfor
endfunction

function! s:errorformat(metalinter) abort
  if a:metalinter == 'golangci-lint'
    " Golangci-lint can output the following:
    "   <file>:<line>:<column>: <message> (<linter>)
    " This can be defined by the following errorformat:
    return 'level=%tarning\ msg="%m:\ [%f:%l:%c:\ %.%#]",level=%tarning\ msg="%m",level=%trror\ msg="%m:\ [%f:%l:%c:\ %.%#]",level=%trror\ msg="%m",%f:%l:%c:\ %m,%f:%l:\ %m,%f:%l\ %m'
  elseif a:metalinter == 'staticcheck'
    return '%f:%l:%c:\ %m'
  elseif a:metalinter == 'gopls'
    let l:efm = ''
    let l:level = go#config#DiagnosticsLevel()

    if l:level == 0
      return '%-G%f:%l:%c:%t:\ %m,%-G%f:%l:%c::\ %m,%-G%f:%l::%t:\ %m'
    endif

    if l:level < 2
      let l:efm = '%-G%f:%l:%c:W:\ %m,%-G%f:%l::W:\ %m,'
    endif
    return l:efm . '%f:%l:%c:%t:\ %m,%f:%l:%c::\ %m,%f:%l::%t:\ %m'
  endif
endfunction

function! s:preserveerrors(autosave, listtype) abort
  return a:autosave && a:listtype == go#list#Type("GoFmt") && go#config#FmtAutosave() && isdirectory(expand('%:p:h'))
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
