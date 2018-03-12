func! Test_Gometa() abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint'
  silent exe 'e ' . $GOPATH . '/src/lint/lint.go'

  let expected = [
        \ {'lnum': 5, 'bufnr': 3, 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'w', 'pattern': '', 'text': 'exported function MissingFooDoc should have comment or be unexported (golint)'}
      \ ]

  " clear the quickfix lists
  call setqflist([], 'r')

  " call go#lint#ToggleMetaLinterAutoSave from lint.vim so that the file will
  " be autoloaded and the default for g:go_metalinter_enabled will be set so
  " we can capture it to restore it after the test is run.
  call go#lint#ToggleMetaLinterAutoSave()
  " And restore it back to its previous value
  call go#lint#ToggleMetaLinterAutoSave()

  let orig_go_metalinter_enabled = g:go_metalinter_enabled
  let g:go_metalinter_enabled = ['golint']

  call go#lint#Gometa(0, $GOPATH . '/src/foo')

  let actual = getqflist()
  let start = reltime()
  while len(actual) == 0 && reltimefloat(reltime(start)) < 10
    sleep 100m
    let actual = getqflist()
  endwhile

  call gotest#assert_quickfix(actual, expected)
  let g:go_metalinter_enabled = orig_go_metalinter_enabled
endfunc

func! Test_GometaWithDisabled() abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint'
  silent exe 'e ' . $GOPATH . '/src/lint/lint.go'

  let expected = [
        \ {'lnum': 5, 'bufnr': 3, 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'w', 'pattern': '', 'text': 'exported function MissingFooDoc should have comment or be unexported (golint)'}
      \ ]

  " clear the quickfix lists
  call setqflist([], 'r')

  " call go#lint#ToggleMetaLinterAutoSave from lint.vim so that the file will
  " be autoloaded and the default for g:go_metalinter_disabled will be set so
  " we can capture it to restore it after the test is run.
  call go#lint#ToggleMetaLinterAutoSave()
  " And restore it back to its previous value
  call go#lint#ToggleMetaLinterAutoSave()

  let orig_go_metalinter_disabled = g:go_metalinter_disabled
  let g:go_metalinter_disabled = ['vet']

  call go#lint#Gometa(0, $GOPATH . '/src/foo')

  let actual = getqflist()
  let start = reltime()
  while len(actual) == 0 && reltimefloat(reltime(start)) < 10
    sleep 100m
    let actual = getqflist()
  endwhile

  call gotest#assert_quickfix(actual, expected)
  let g:go_metalinter_disabled = orig_go_metalinter_disabled
endfunc

func! Test_GometaAutoSave() abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/lint'
  silent exe 'e ' . $GOPATH . '/src/lint/lint.go'

  let expected = [
        \ {'lnum': 5, 'bufnr': 2, 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': 'w', 'pattern': '', 'text': 'exported function MissingDoc should have comment or be unexported (golint)'}
      \ ]

  let winnr = winnr()

  " clear the location lists
  call setloclist(l:winnr, [], 'r')

  " call go#lint#ToggleMetaLinterAutoSave from lint.vim so that the file will
  " be autoloaded and the default for g:go_metalinter_autosave_enabled will be
  " set so we can capture it to restore it after the test is run.
  call go#lint#ToggleMetaLinterAutoSave()
  " And restore it back to its previous value
  call go#lint#ToggleMetaLinterAutoSave()

  let orig_go_metalinter_autosave_enabled = g:go_metalinter_autosave_enabled
  let g:go_metalinter_autosave_enabled = ['golint']

  call go#lint#Gometa(1)

  let actual = getloclist(l:winnr)
  let start = reltime()
  while len(actual) == 0 && reltimefloat(reltime(start)) < 10
    sleep 100m
    let actual = getloclist(l:winnr)
  endwhile

  call gotest#assert_quickfix(actual, expected)
  let g:go_metalinter_autosave_enabled = orig_go_metalinter_autosave_enabled
endfunc

" vim: sw=2 ts=2 et
