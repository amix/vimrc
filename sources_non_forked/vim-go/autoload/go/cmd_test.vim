" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_GoBuildErrors()
  try
    let l:filename = 'cmd/bad.go'
    let l:tmp = gotest#load_fixture(l:filename)
    exe 'cd ' . l:tmp . '/src/cmd'

    " set the compiler type so that the errorformat option will be set
    " correctly.
    compiler go

    let expected = [{'lnum': 4, 'bufnr': bufnr('%'), 'col': 2, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'undefined: notafunc'}]
    " clear the quickfix lists
    call setqflist([], 'r')

    call go#cmd#Build(1)

    let actual = getqflist()
    let start = reltime()
    while len(actual) == 0 && reltimefloat(reltime(start)) < 10
      sleep 100m
      let actual = getqflist()
    endwhile

    call gotest#assert_quickfix(actual, l:expected)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
