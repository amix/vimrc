" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_Gometa() abort
  call s:gometa('gometaliner')
endfunc

func! Test_GometaGolangciLint() abort
  call s:gometa('golangci-lint')
endfunc

func! s:gometa(metalinter) abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint'
  silent exe 'e ' . $GOPATH . '/src/lint/lint.go'

  try
    let g:go_metalinter_comand = a:metalinter
    let expected = [
          \ {'lnum': 5, 'bufnr': bufnr('%')+1, 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'w', 'pattern': '', 'text': 'exported function MissingFooDoc should have comment or be unexported (golint)'}
        \ ]

    " clear the quickfix lists
    call setqflist([], 'r')

    let g:go_metalinter_enabled = ['golint']

    call go#lint#Gometa(0, 0, $GOPATH . '/src/foo')

    let actual = getqflist()
    let start = reltime()
    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
      sleep 100m
      let actual = getqflist()
    endwhile

    call gotest#assert_quickfix(actual, expected)
  finally
      unlet g:go_metalinter_enabled
  endtry
endfunc

func! Test_GometaWithDisabled() abort
  call s:gometawithdisabled('gometalinter')
endfunc

func! Test_GometaWithDisabledGolangciLint() abort
  call s:gometawithdisabled('golangci-lint')
endfunc

func! s:gometawithdisabled(metalinter) abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint'
  silent exe 'e ' . $GOPATH . '/src/lint/lint.go'

  try
    let g:go_metalinter_comand = a:metalinter
    let expected = [
          \ {'lnum': 5, 'bufnr': bufnr('%')+1, 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'w', 'pattern': '', 'text': 'exported function MissingFooDoc should have comment or be unexported (golint)'}
        \ ]

    " clear the quickfix lists
    call setqflist([], 'r')

    let g:go_metalinter_disabled = ['vet']

    call go#lint#Gometa(0, 0, $GOPATH . '/src/foo')

    let actual = getqflist()
    let start = reltime()
    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
      sleep 100m
      let actual = getqflist()
    endwhile

    call gotest#assert_quickfix(actual, expected)
  finally
    unlet g:go_metalinter_disabled
  endtry
endfunc

func! Test_GometaAutoSave() abort
  call s:gometaautosave('gometalinter')
endfunc

func! Test_GometaAutoSaveGolangciLint() abort
  call s:gometaautosave('golangci-lint')
endfunc

func! s:gometaautosave(metalinter) abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint'
  silent exe 'e ' . $GOPATH . '/src/lint/lint.go'

  try
    let g:go_metalinter_comand = a:metalinter
    let expected = [
          \ {'lnum': 5, 'bufnr': bufnr('%'), 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'w', 'pattern': '', 'text': 'exported function MissingDoc should have comment or be unexported (golint)'}
        \ ]

    let winnr = winnr()

    " clear the location lists
    call setloclist(l:winnr, [], 'r')

    let g:go_metalinter_autosave_enabled = ['golint']

    call go#lint#Gometa(0, 1)

    let actual = getloclist(l:winnr)
    let start = reltime()
    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
      sleep 100m
      let actual = getloclist(l:winnr)
    endwhile

    call gotest#assert_quickfix(actual, expected)
  finally
    unlet g:go_metalinter_autosave_enabled
  endtry
endfunc

func! Test_Vet() abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint'
  silent exe 'e ' . $GOPATH . '/src/vet/vet.go'
  compiler go

  let expected = [
        \ {'lnum': 7, 'bufnr': bufnr('%'), 'col': 2, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '',
        \ 'text': 'Printf format %d has arg str of wrong type string'}
      \ ]

  let winnr = winnr()

  " clear the location lists
  call setqflist([], 'r')

  call go#lint#Vet(1)

  let actual = getqflist()
  let start = reltime()
  while len(actual) == 0 && reltimefloat(reltime(start)) < 10
    sleep 100m
    let actual = getqflist()
  endwhile

  call gotest#assert_quickfix(actual, expected)
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
