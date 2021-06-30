" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! Test_GoDebugStart_Empty() abort
  call s:debug()
endfunction

function! Test_GoDebugStart_RelativePackage() abort
  call s:debug('./debug/debugmain')
endfunction

function! Test_GoDebugStart_RelativePackage_NullModule() abort
  call s:debug('./debug/debugmain', 1)
endfunction

function! Test_GoDebugStart_Package() abort
  call s:debug('debug/debugmain')
endfunction

function! Test_GoDebugStart_Errors() abort
  if !go#util#has_job()
    return
  endif

  try
    let l:expected = [
          \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 0, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': '# debug/compilerror'},
          \ {'lnum': 6, 'bufnr': 7, 'col': 22, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': ' syntax error: unexpected newline, expecting comma or )'},
          \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 0, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'exit status 2'}
          \]
    call setqflist([], 'r')

    let l:tmp = gotest#load_fixture('debug/compilerror/main.go')
    call assert_false(exists(':GoDebugStop'))

    let l:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
    execute l:cd . ' debug/compilerror'

    call go#debug#Start(0)

    let l:actual = getqflist()
    let l:start = reltime()
    while len(l:actual) == 0 && reltimefloat(reltime(l:start)) < 10
      sleep 100m
      let l:actual = getqflist()
    endwhile

    call gotest#assert_quickfix(l:actual, l:expected)
    call assert_false(exists(':GoDebugStop'))

  finally
    call delete(l:tmp, 'rf')
    " clear the quickfix lists
    call setqflist([], 'r')
  endtry
endfunction

" s:debug takes 2 optional arguments. The first is a package to debug. The
" second is a flag to indicate whether to reset GOPATH after
" gotest#load_fixture is called in order to test behavior outside of GOPATH.
function! s:debug(...) abort
  if !go#util#has_job()
    return
  endif

  try
    let $oldgopath = $GOPATH
    let l:tmp = gotest#load_fixture('debug/debugmain/debugmain.go')

    if a:0 > 1 && a:2 == 1
      let $GOPATH = $oldgopath
    endif

    call go#debug#Breakpoint(6)

    call assert_false(exists(':GoDebugStop'))

    if a:0 == 0
      let l:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
      execute l:cd . ' debug/debugmain'
      let l:job = go#debug#Start(0)
    else
      let l:job = go#debug#Start(0, a:1)
    endif

    let l:start = reltime()
    while !exists(':GoDebugStop') && reltimefloat(reltime(l:start)) < 10
      sleep 100m
    endwhile

    call assert_true(exists(':GoDebugStop'))
    call gotest#assert_quickfix(getqflist(), [])

    call go#debug#Stop()

    if !has('nvim')
      call assert_equal(job_status(l:job), 'dead')
    endif

    call assert_false(exists(':GoDebugStop'))

  finally
    call go#debug#Breakpoint(6)
    call delete(l:tmp, 'rf')
  endtry
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
