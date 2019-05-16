" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_Complete_GOPATH_simple() abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/package'
  silent exe 'edit ' . $GOPATH . '/src/package/package.go'
  call s:complete('package', ['package'])
endfunc

func! Test_Complete_Module_simple() abort
  silent exe 'edit ' . fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/package/src/package/package.go'
  call s:complete('package', ['package'])
endfunc

func! Test_Complete_GOPATH_subdirs() abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/package'
  silent exe 'edit ' . $GOPATH . '/src/package/package.go'
  call s:complete('package/', ['package/bar', 'package/baz'])
endfunc

func! Test_Complete_Module_subdirs() abort
  silent exe 'edit ' . fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/package/src/package/package.go'
  call s:complete('package/', ['package/bar', 'package/baz'])
endfunc

func! Test_Complete_GOPATH_baronly() abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/package'
  silent exe 'edit ' . $GOPATH . '/src/package/package.go'
  call s:complete('package/bar', ['package/bar'])
endfunc

func! Test_Complete_Module_baronly() abort
  silent exe 'edit ' . fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/package/src/package/package.go'
  call s:complete('package/bar', ['package/bar'])
endfunc

func! Test_Complete_GOPATH_vendor() abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/package'
  silent exe 'edit ' . $GOPATH . '/src/package/package.go'
  call s:complete('foo', ['foo'])
endfunc

func! Test_Complete_Module_vendor() abort
  silent exe 'edit ' . fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/package/src/package/package.go'
  call s:complete('foo', ['foo'])
endfunc

func! s:complete(arglead, expected) abort
  let l:candidates = go#package#Complete(a:arglead, '', 1)
  call assert_equal(a:expected, l:candidates)
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
