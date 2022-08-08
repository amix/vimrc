" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_GometaGolangciLint() abort
  call s:gometa('golangci-lint')
endfunc

func! Test_GometaStaticcheck() abort
  call s:gometa('staticcheck')
endfunc

func! s:gometa(metalinter) abort
  let RestoreGOPATH = go#util#SetEnv('GOPATH', fnamemodify(getcwd(), ':p') . 'test-fixtures/lint')
  call go#util#Chdir('test-fixtures/lint/src/foo')
  silent exe 'e! ' . $GOPATH . '/src/foo/foo.go'

  try
    let g:go_metalinter_command = a:metalinter
    let l:vim = s:vimdir()
    let expected = [
          \ {'lnum': 1, 'bufnr': bufnr('%'), 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'at least one file in a package should have a package comment (ST1000)'},
        \ ]
    if a:metalinter == 'golangci-lint'
      let expected = [
            \ {'lnum': 5, 'bufnr': bufnr('%'), 'col': 1, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': 'exported: exported function MissingFooDoc should have comment or be unexported (revive)'}
          \ ]
    endif

    " clear the quickfix list
    call setqflist([], 'r')

    let g:go_metalinter_enabled = ['ST1000']
    if a:metalinter == 'golangci-lint'
      let g:go_metalinter_enabled = ['revive']
    endif

    call go#lint#Gometa(0, 0, $GOPATH . '/src/foo')

    let actual = getqflist()
    let start = reltime()
    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
      sleep 100m
      let actual = copy(getqflist())
    endwhile

    " sort the results, because golangci-lint doesn't always return notices in
    " a deterministic order.
    call sort(l:actual)
    call sort(l:expected)

    call gotest#assert_quickfix(actual, expected)
  finally
      call call(RestoreGOPATH, [])
      unlet g:go_metalinter_enabled
      unlet g:go_metalinter_command
  endtry
endfunc

"func! Test_GometaGolangciLint_shadow() abort
"  call s:gometa_shadow('golangci-lint')
"endfunc
"
"func! s:gometa_shadow(metalinter) abort
"  let RestoreGOPATH = go#util#SetEnv('GOPATH', fnamemodify(getcwd(), ':p') . 'test-fixtures/lint')
"  silent exe 'e! ' . $GOPATH . '/src/lint/golangci-lint/problems/shadow/problems.go'
"
"  try
"    let g:go_metalinter_command = a:metalinter
"    let expected = [
"          \ {'lnum': 4, 'bufnr': bufnr('%'), 'col': 7, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'w', 'module': '', 'text': '[runner] Can''t run linter golint: golint: analysis skipped: errors in package'},
"          \ {'lnum': 4, 'bufnr': bufnr('%'), 'col': 7, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'e', 'module': '', 'text': 'Running error: golint: analysis skipped: errors in package'}
"        \ ]
"
"    " clear the quickfix list
"    call setqflist([], 'r')
"
"    let g:go_metalinter_enabled = ['golint']
"
"    call go#lint#Gometa(0, 0)
"
"    let actual = getqflist()
"    let start = reltime()
"    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
"      sleep 100m
"      let actual = getqflist()
"    endwhile
"
"    call gotest#assert_quickfix(actual, expected)
"  finally
"      call call(RestoreGOPATH, [])
"      unlet g:go_metalinter_enabled
"      unlet g:go_metalinter_command
"  endtry
"endfunc

func! Test_GometaAutoSaveGolangciLint() abort
  call s:gometaautosave('golangci-lint', 0)
endfunc

func! Test_GometaAutoSaveStaticcheck() abort
  call s:gometaautosave('staticcheck', 0)
endfunc

func! Test_GometaAutoSaveGopls() abort
  let g:go_gopls_staticcheck = 1
  let g:go_diagnostics_level = 2
  call s:gometaautosave('gopls', 0)
  unlet g:go_gopls_staticcheck
  unlet g:go_diagnostics_level
endfunc

func! Test_GometaAutoSaveGolangciLintKeepsErrors() abort
  call s:gometaautosave('golangci-lint', 1)
endfunc

func! Test_GometaAutoSaveStaticcheckKeepsErrors() abort
  call s:gometaautosave('staticcheck', 1)
endfunc

func! s:gometaautosave(metalinter, withList) abort
  let l:tmp = gotest#load_fixture('lint/src/lint/lint.go')

  try
    let g:go_metalinter_command = a:metalinter
    let l:vim = s:vimdir()
    let l:expected = [
          \ {'lnum': 1, 'bufnr': bufnr('%'), 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'at least one file in a package should have a package comment (ST1000)'},
        \ ]
    if a:metalinter == 'gopls'
      let l:expected = []
"      let l:expected = [
"            \ {'lnum': 1, 'bufnr': bufnr('%'), 'col': 1, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'W', 'module': '', 'text': 'at least one file in a package should have a package comment'}
"          \ ]
    elseif a:metalinter == 'golangci-lint'
      let l:expected = [
            \ {'lnum': 5, 'bufnr': bufnr('%'), 'col': 1, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': 'exported: exported function MissingDoc should have comment or be unexported (revive)'}
          \ ]
    endif

    let l:list = []
    if a:withList
      let l:list = [
            \ {'lnum': 1, 'bufnr': bufnr('%'), 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'before metalinter'}
          \ ]
      let l:expected = extend(copy(l:list), l:expected)
    endif

    " set the location list
    call setloclist(0, l:list, 'r')

    let g:go_metalinter_autosave_enabled = ['ST1000']
    if a:metalinter == 'golangci-lint'
      let g:go_metalinter_autosave_enabled = ['revive']
    endif

    call go#lint#Gometa(0, 1)

    let l:actual = getloclist(0)
    let l:start = reltime()
    while len(l:actual) != len(l:expected) && reltimefloat(reltime(l:start)) < 10
      sleep 100m
      let l:actual = copy(getloclist(0))
    endwhile

    " sort the results, because golangci-lint doesn't always return notices in
    " a deterministic order.
    call sort(l:actual)
    call sort(l:expected)

    call gotest#assert_quickfix(l:actual, l:expected)
  finally
    call delete(l:tmp, 'rf')
    unlet g:go_metalinter_autosave_enabled
    unlet g:go_metalinter_command
  endtry
endfunc

"func! Test_GometaGolangciLint_importabs() abort
"  call s:gometa_importabs('golangci-lint')
"endfunc
"
"func! s:gometa_importabs(metalinter) abort
"  let RestoreGOPATH = go#util#SetEnv('GOPATH', fnamemodify(getcwd(), ':p') . 'test-fixtures/lint')
"  silent exe 'e! ' . $GOPATH . '/src/lint/golangci-lint/problems/importabs/problems.go'
"
"  try
"    let g:go_metalinter_command = a:metalinter
"
"    let expected = [
"          \ {'lnum': 3, 'bufnr': bufnr('%'), 'col': 8, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': '"/quux" imported but not used (typecheck)'},
"        \ ]
"    " clear the quickfix list
"    call setqflist([], 'r')
"
"    let g:go_metalinter_enabled = ['revive', 'typecheck']
"
"    call go#lint#Gometa(0, 0)
"
"    let actual = getqflist()
"    let start = reltime()
"    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
"      sleep 100m
"      let actual = copy(getqflist())
"    endwhile
"
"    " sort the results, because golangci-lint doesn't always return notices in
"    " a deterministic order.
"    call sort(l:actual)
"    call sort(l:expected)
"
"    call gotest#assert_quickfix(actual, expected)
"  finally
"      call call(RestoreGOPATH, [])
"      unlet g:go_metalinter_enabled
"      unlet g:go_metalinter_command
"  endtry
"endfunc
"
"func! Test_GometaAutoSaveGolangciLint_importabs() abort
"  call s:gometaautosave_importabs('golangci-lint')
"endfunc
"
"func! s:gometaautosave_importabs(metalinter) abort
"  let RestoreGOPATH = go#util#SetEnv('GOPATH', fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint')
"  silent exe 'e! ' . $GOPATH . '/src/lint/golangci-lint/problems/importabs/ok.go'
"
"  try
"    let g:go_metalinter_command = a:metalinter
"    let expected = [
"          \ {'lnum': 3, 'bufnr': bufnr('%')+1, 'col': 8, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'w', 'module': '', 'text': '[runner] Can''t run linter golint: golint: analysis skipped: errors in package'},
"          \ {'lnum': 3, 'bufnr': bufnr('%')+1, 'col': 8, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'e', 'module': '', 'text': 'Running error: golint: analysis skipped: errors in package'}
"        \ ]
"
"    " clear the location list
"    call setloclist(0, [], 'r')
"
"    let g:go_metalinter_autosave_enabled = ['golint']
"
"    call go#lint#Gometa(0, 1)
"
"    let actual = getloclist(0)
"    let start = reltime()
"    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
"      sleep 100m
"      let actual = getloclist(0)
"    endwhile
"
"    call gotest#assert_quickfix(actual, expected)
"  finally
"    call call(RestoreGOPATH, [])
"    unlet g:go_metalinter_autosave_enabled
"    unlet g:go_metalinter_command
"  endtry
"endfunc
"
"func! Test_GometaGolangciLint_multiple() abort
"  call s:gometa_multiple('golangci-lint')
"endfunc

func! s:gometa_multiple(metalinter) abort
  let RestoreGOPATH = go#util#SetEnv('GOPATH', fnamemodify(getcwd(), ':p') . 'test-fixtures/lint')
  silent exe 'e! ' . $GOPATH . '/src/lint/golangci-lint/problems/multiple/problems.go'

  try
    let g:go_metalinter_command = a:metalinter
    let expected = [
          \ {'lnum': 8, 'bufnr': bufnr('%'), 'col': 7, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': 'time.Sleep undefined (type int has no field or method Sleep) (typecheck)'},
          \ {'lnum': 4, 'bufnr': bufnr('%'), 'col': 2, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': '"time" imported but not used (typecheck)'}
        \ ]

    " clear the quickfix list
    call setqflist([], 'r')

    let g:go_metalinter_enabled = ['revive', 'typecheck']

    call go#lint#Gometa(0, 0)

    let actual = getqflist()
    let start = reltime()
    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
      sleep 100m
      let actual = copy(getqflist())
    endwhile

    " sort the results, because golangci-lint doesn't always return notices in
    " a deterministic order.
    call sort(l:actual)
    call sort(l:expected)

    call gotest#assert_quickfix(actual, expected)
  finally
      call call(RestoreGOPATH, [])
      unlet g:go_metalinter_enabled
      unlet g:go_metalinter_command
  endtry
endfunc

func! Test_GometaAutoSaveGolangciLint_multiple() abort
  call s:gometaautosave_multiple('golangci-lint')
endfunc

func! s:gometaautosave_multiple(metalinter) abort
  let RestoreGOPATH = go#util#SetEnv('GOPATH', fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint')
  silent exe 'e! ' . $GOPATH . '/src/lint/golangci-lint/problems/multiple/problems.go'

  try
    let g:go_metalinter_command = a:metalinter
    let expected = [
          \ {'lnum': 8, 'bufnr': bufnr('%'), 'col': 7, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': 'time.Sleep undefined (type int has no field or method Sleep) (typecheck)'},
          \ {'lnum': 4, 'bufnr': bufnr('%'), 'col': 2, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': '"time" imported but not used (typecheck)'}
        \ ]

    " clear the location list
    call setloclist(0, [], 'r')

    let g:go_metalinter_autosave_enabled = ['revive', 'typecheck']

    call go#lint#Gometa(0, 1)

    let actual = getloclist(0)
    let start = reltime()
    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
      sleep 100m
      let actual = copy(getloclist(0))
    endwhile

    " sort the results, because golangci-lint doesn't always return notices in
    " a deterministic order.
    call sort(l:actual)
    call sort(l:expected)

    call gotest#assert_quickfix(actual, expected)
  finally
    call call(RestoreGOPATH, [])
    unlet g:go_metalinter_autosave_enabled
    unlet g:go_metalinter_command
  endtry
endfunc

func! Test_Vet() abort
  let l:tmp = gotest#load_fixture('lint/src/vet/vet.go')

  try
    let expected = [
          \ {'lnum': 7, 'bufnr': bufnr('%'), 'col': 2, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'fmt.Printf format %d has arg str of wrong type string'}
        \ ]

    let [l:goversion, l:err] = go#util#Exec(['go', 'env', 'GOVERSION'])
    let l:goversion = split(l:goversion, "\n")[0]
    if l:goversion < 'go1.18'
      let expected = [
            \ {'lnum': 7, 'bufnr': bufnr('%'), 'col': 2, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'Printf format %d has arg str of wrong type string'}
          \ ]
    endif

    let winnr = winnr()

    " clear the quickfix list
    call setqflist([], 'r')

    call go#lint#Vet(1)

    let actual = getqflist()
    let start = reltime()
    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
      sleep 100m
      let actual = getqflist()
    endwhile

    call gotest#assert_quickfix(actual, expected)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_Vet_subdir() abort
  let l:tmp = gotest#load_fixture('lint/src/vet/vet.go')

  " go up one directory to easily test that go vet's file paths are handled
  " correctly when the working directory is not the directory that contains
  " the file being vetted.
  call go#util#Chdir('..')

  try
    let expected = [
          \ {'lnum': 7, 'bufnr': bufnr('%'), 'col': 2, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'fmt.Printf format %d has arg str of wrong type string'}
        \ ]

    let [l:goversion, l:err] = go#util#Exec(['go', 'env', 'GOVERSION'])
    let l:goversion = split(l:goversion, "\n")[0]
    if l:goversion < 'go1.18'
      let expected = [
            \ {'lnum': 7, 'bufnr': bufnr('%'), 'col': 2, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'Printf format %d has arg str of wrong type string'}
          \ ]
    endif

    let winnr = winnr()

    " clear the quickfix list
    call setqflist([], 'r')

    call go#lint#Vet(1)

    let actual = getqflist()
    let start = reltime()
    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
      sleep 100m
      let actual = getqflist()
    endwhile

    call gotest#assert_quickfix(actual, expected)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_Vet_compilererror() abort
  let l:tmp = gotest#load_fixture('lint/src/vet/compilererror/compilererror.go')

  try
    let expected = [
          \ {'lnum': 6, 'bufnr': bufnr('%'), 'col': 22, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': "missing ',' before newline in argument list (and 1 more errors)"}
        \ ]

    let winnr = winnr()

    " clear the quickfix list
    call setqflist([], 'r')

    call go#lint#Vet(1)

    let actual = getqflist()
    let start = reltime()
    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
      sleep 100m
      let actual = getqflist()
    endwhile

    call gotest#assert_quickfix(actual, expected)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_Lint_GOPATH() abort
  let RestoreGO111MODULE = go#util#SetEnv('GO111MODULE', 'off')
  let RestoreGOPATH = go#util#SetEnv('GOPATH', fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint')

  silent exe 'e! ' . $GOPATH . '/src/lint/quux.go'
  silent exe 'e! ' . $GOPATH . '/src/lint/lint.go'
  compiler go

  let expected = [
          \ {'lnum': 1, 'bufnr': bufnr('%')+9, 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'should have a package comment'},
          \ {'lnum': 5, 'bufnr': bufnr('%'), 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'exported function MissingDoc should have comment or be unexported'},
          \ {'lnum': 5, 'bufnr': bufnr('test-fixtures/lint/src/lint/quux.go'), 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'exported function AlsoMissingDoc should have comment or be unexported'}
      \ ]

  let winnr = winnr()

  " clear the quickfix list
  call setqflist([], 'r')

  call go#lint#Golint(1)

  let actual = getqflist()
  let start = reltime()
  while len(actual) == 0 && reltimefloat(reltime(start)) < 10
    sleep 100m
    let actual = getqflist()
  endwhile

  " sort the results for deterministic ordering
  call sort(actual)
  call sort(expected)

  call gotest#assert_quickfix(actual, expected)

  "call assert_report(execute('ls'))

  call call(RestoreGOPATH, [])
  call call(RestoreGO111MODULE, [])
endfunc

func! Test_Lint_NullModule() abort
  let RestoreGO111MODULE = go#util#SetEnv('GO111MODULE', 'off')
  silent exe 'e! ' . fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint/src/lint/quux.go'
  silent exe 'e! ' . fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint/src/lint/lint.go'
  compiler go

  let expected = [
          \ {'lnum': 1, 'bufnr': bufnr('%')+9, 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'should have a package comment'},
          \ {'lnum': 5, 'bufnr': bufnr('%'), 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'exported function MissingDoc should have comment or be unexported'},
          \ {'lnum': 5, 'bufnr': bufnr('test-fixtures/lint/src/lint/quux.go'), 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'exported function AlsoMissingDoc should have comment or be unexported'}
      \ ]

  let winnr = winnr()

  " clear the quickfix list
  call setqflist([], 'r')

  call go#lint#Golint(1)

  let actual = getqflist()
  let start = reltime()
  while len(actual) == 0 && reltimefloat(reltime(start)) < 10
    sleep 100m
    let actual = getqflist()
  endwhile

  " sort the results for deterministic ordering
  call sort(actual)
  call sort(expected)

  call gotest#assert_quickfix(actual, expected)
  call call(RestoreGO111MODULE, [])
endfunc

func! Test_Errcheck() abort
  let RestoreGOPATH = go#util#SetEnv('GOPATH', fnamemodify(getcwd(), ':p') . 'test-fixtures/lint')
  silent exe 'e! ' . $GOPATH . '/src/errcheck/errcheck.go'

  try
    let l:bufnr = bufnr('')
    let expected = [
          \ {'lnum': 9, 'bufnr': bufnr(''), 'col': 9, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': ":\tio.Copy(os.Stdout, os.Stdin)"},
          \ {'lnum': 10, 'bufnr': bufnr('')+1, 'col': 9, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': ":\tio.Copy(os.Stdout, os.Stdin)"},
        \ ]

    " clear the quickfix list
    call setqflist([], 'r')

    call go#lint#Errcheck(1)

    call gotest#assert_quickfix(getqflist(), expected)
    call assert_equal(l:bufnr, bufnr(''))
  finally
    call call(RestoreGOPATH, [])
  endtry
endfunc

func! Test_Errcheck_options() abort
  let RestoreGOPATH = go#util#SetEnv('GOPATH', fnamemodify(getcwd(), ':p') . 'test-fixtures/lint')
  silent exe 'e! ' . $GOPATH . '/src/errcheck/errcheck.go'

  try
    let l:bufnr = bufnr('')
    let expected = [
          \ {'lnum': 9, 'bufnr': bufnr(''), 'col': 9, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': ":\tio.Copy(os.Stdout, os.Stdin)"},
        \ ]

    " clear the quickfix list
    call setqflist([], 'r')

    call go#lint#Errcheck(1, '-ignoretests')

    call gotest#assert_quickfix(getqflist(), expected)
    call assert_equal(l:bufnr, bufnr(''))
  finally
    call call(RestoreGOPATH, [])
  endtry
endfunc

func! Test_Errcheck_compilererror() abort
  let l:tmp = gotest#load_fixture('lint/src/errcheck/compilererror/compilererror.go')

  try
    let l:bufnr = bufnr('')
    let expected = []

    " clear the quickfix list
    call setqflist([], 'r')

    call go#lint#Errcheck(1)

    call gotest#assert_quickfix(getqflist(), expected)
    call assert_equal(l:bufnr, bufnr(''))
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! s:vimdir()
  let l:vim = "vim-8.2"
  if has('nvim')
    let l:vim = 'nvim'
  elseif v:version == 800
    let l:vim = 'vim-8.0'
  endif

  return l:vim
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
