" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_JobDirWithSpaces()
  if !go#util#has_job()
    return
  endif

  try
    let l:filename = 'job/dir has spaces/main.go'
    let l:tmp = gotest#load_fixture(l:filename)
    exe 'cd ' . fnameescape(l:tmp . '/src/job/dir has spaces')

    " set the compiler type so that the errorformat option will be set
    " correctly.
    compiler go

    let expected = [{'lnum': 4, 'bufnr': bufnr('%'), 'col': 2, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'undefined: notafunc'}]
    " clear the quickfix lists
    call setqflist([], 'r')

    " go build discards any results when it compiles multiple packages. So we
    " pass the `errors` package just as a placeholder with the current folder
    " (indicated with '.').
    let l:cmd = ['go', 'build', '.', 'errors']

    let l:complete = go#promise#New(function('s:complete'), 10000, '')
    call go#job#Spawn(l:cmd, {
          \ 'for': 'GoBuild',
          \ 'complete': l:complete.wrapper,
          \ 'statustype': 'build'
         \})

    let l:out = l:complete.await()

    let actual = getqflist()

    call gotest#assert_quickfix(actual, l:expected)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! s:complete(job, exit_code, messages)
  return a:messages
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
