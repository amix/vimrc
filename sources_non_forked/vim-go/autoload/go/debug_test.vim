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
  call s:debug('vim-go.test/debug/debugmain')
endfunction

function! Test_GoDebugStart_Errors() abort
  if !go#util#has_job()
    return
  endif

  try
    let l:tmp = gotest#load_fixture('debug/compilerror/main.go')

    let l:expected = [
          \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 0, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': '# vim-go.test/debug/compilerror'},
          \ {'lnum': 6, 'bufnr': bufnr('%'), 'col': 22, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': ' syntax error: unexpected newline in argument list; possibly missing comma or )'},
          \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 0, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'exit status 2'}
          \]
  let [l:goversion, l:err] = go#util#Exec(['go', 'env', 'GOVERSION'])
  let l:goversion = split(l:goversion, "\n")[0]
  if l:goversion < 'go1.19'
    let expected = [
          \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 0, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': '# vim-go.test/debug/compilerror'},
          \ {'lnum': 6, 'bufnr': bufnr('%'), 'col': 22, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': ' syntax error: unexpected newline, expecting comma or )'},
          \ {'lnum': 0, 'bufnr': 0, 'col': 0, 'valid': 0, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'exit status 2'}
        \ ]
  endif
    call setqflist([], 'r')

    call assert_false(exists(':GoDebugStop'))

    call go#util#Chdir('debug/compilerror')

    call go#debug#Start('debug')

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

function! Test_GoDebugModeRemapsAndRestoresKeys() abort
  if !go#util#has_job()
    return
  endif

  try
    let g:go_debug_mappings = {'(go-debug-continue)': {'key': 'q', 'arguments': '<nowait>'}}
    let l:tmp = gotest#load_fixture('debug/debugmain/debugmain.go')

    call assert_false(exists(':GoDebugStop'))

    call go#util#Chdir('debug/debugmain')

    call go#debug#Start('debug')

    let l:start = reltime()
    while maparg('q') == '' && reltimefloat(reltime(l:start)) < 10
      sleep 100m
    endwhile

    call assert_false(exists(':GoDebugStart'))
    call assert_equal('<Plug>(go-debug-continue)', maparg('q', 'n', 0))

    call go#debug#Stop()
    while exists(':GoDebugStop') && reltimefloat(reltime(l:start)) < 10
      sleep 100m
    endwhile
    call assert_equal('', maparg('q'))
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunction

function! Test_GoDebugStopRemovesPlugMappings() abort
  if !go#util#has_job()
    return
  endif

  try
    let l:tmp = gotest#load_fixture('debug/debugmain/debugmain.go')

    call assert_false(exists(':GoDebugStop'))

    call go#util#Chdir('debug/debugmain')

    call go#debug#Start('debug')

    let l:start = reltime()
    while maparg('<Plug>(go-debug-stop)') == '' && reltimefloat(reltime(l:start)) < 10
      sleep 100m
    endwhile

    call assert_false(exists(':GoDebugStart'))
    call assert_equal(':<C-U>call go#debug#Stop()<CR>', maparg('<Plug>(go-debug-stop)', 'n', 0))

    call go#debug#Stop()
    while exists(':GoDebugStop') && reltimefloat(reltime(l:start)) < 10
      sleep 100m
    endwhile
    call assert_equal('', maparg('<Plug>(go-debug-stop'))
  finally
    call delete(l:tmp, 'rf')
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
      call go#util#Chdir(printf('%s/src/debug/debugmain', l:tmp))
      let l:job = go#debug#Start('debug')
    else
      let l:job = go#debug#Start('debug', a:1)
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
