" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_GoTest() abort
  let expected = [
        \ {'lnum': 12, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'log message'},
        \ {'lnum': 13, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'sub badness'},
        \ {'lnum': 15, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'badness'},
        \ {'lnum': 16, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'helper badness'},
        \ {'lnum': 20, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'this is an error'},
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'and a second line, too'},
        \ {'lnum': 21, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': ''},
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'this is another error'},
        \ {'lnum': 26, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'this is a sub-test error'},
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'and a second line, too'},
        \ {'lnum': 6, 'bufnr': 3, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'another package badness'},
        \ {'lnum': 43, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'panic: worst ever [recovered]'}
      \ ]
  call s:test('play/play_test.go', expected)
endfunc

func! Test_GoTestConcurrentPanic()
  let expected = [
        \ {'lnum': 50, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'panic: concurrent fail'}
      \ ]
  call s:test('play/play_test.go', expected, "-run", "TestConcurrentPanic")
endfunc

func! Test_GoTestVerbose() abort
  let expected = [
        \ {'lnum': 12, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'log message'},
        \ {'lnum': 13, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'sub badness'},
        \ {'lnum': 15, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'badness'},
        \ {'lnum': 16, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'helper badness'},
        \ {'lnum': 20, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'this is an error'},
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'and a second line, too'},
        \ {'lnum': 21, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': ''},
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'this is another error'},
        \ {'lnum': 26, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'this is a sub-test error'},
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'and a second line, too'},
        \ {'lnum': 32, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'goodness'},
        \ {'lnum': 6, 'bufnr': 3, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'another package badness'},
        \ {'lnum': 43, 'bufnr': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'panic: worst ever [recovered]'}
      \ ]
  call s:test('play/play_test.go', expected, "-v")
endfunc

func! Test_GoTestCompilerError() abort
  let expected = [
        \ {'lnum': 6, 'bufnr': 6, 'col': 22, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'syntax error: unexpected newline, expecting comma or )'}
      \ ]
  call s:test('compilerror/compilerror_test.go', expected)
endfunc

func! Test_GoTestTimeout() abort
  let expected = [
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'panic: test timed out after 500ms'}
      \ ]

  let g:go_test_timeout="500ms"
  call s:test('timeout/timeout_test.go', expected)
  unlet g:go_test_timeout
endfunc

func! Test_GoTestShowName() abort
  let expected = [
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'TestHelloWorld'},
        \ {'lnum': 6, 'bufnr': 8, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'so long'},
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'TestHelloWorld/sub'},
        \ {'lnum': 9, 'bufnr': 8, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'thanks for all the fish'},
      \ ]

  let g:go_test_show_name=1
  call s:test('showname/showname_test.go', expected)
  unlet g:go_test_show_name
endfunc

func! Test_GoTestVet() abort
  let expected = [
        \ {'lnum': 6, 'bufnr': 11, 'col': 2, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'Errorf format %v reads arg #1, but call has 0 args'},
      \ ]
  call s:test('veterror/veterror.go', expected)
endfunc

func! Test_GoTestTestCompilerError() abort
  let expected = [
        \ {'lnum': 10, 'bufnr': 9, 'col': 16, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'cannot use r (type struct {}) as type io.Reader in argument to ioutil.ReadAll:'},
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'struct {} does not implement io.Reader (missing Read method)'}
      \ ]

  call s:test('testcompilerror/testcompilerror_test.go', expected)
endfunc

func! Test_GoTestExample() abort
  let expected = [
        \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'ExampleHelloWorld'}
      \ ]
  call s:test('example/example_test.go', expected)
endfunc

func! s:test(file, expected, ...) abort
  let $GOPATH = fnameescape(fnamemodify(getcwd(), ':p')) . 'test-fixtures/test'
  silent exe 'e ' . $GOPATH . '/src/' . a:file

  " clear the quickfix lists
  call setqflist([], 'r')

  let args = [1,0]
  if a:0
    let args += a:000
  endif

  " run the tests
  silent call call(function('go#test#Test'), args)

  let actual = getqflist()
  let start = reltime()
  while len(actual) == 0 && reltimefloat(reltime(start)) < 10
    sleep 100m
    let actual = getqflist()
  endwhile

  for item in actual
    let item.text = s:normalize_durations(item.text)
  endfor

  for item in a:expected
    let item.text = s:normalize_durations(item.text)
  endfor

  call gotest#assert_quickfix(actual, a:expected)
endfunc

func! s:normalize_durations(str) abort
  return substitute(a:str, '[0-9]\+\(\.[0-9]\+\)\?s', '0.000s', 'g')
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
