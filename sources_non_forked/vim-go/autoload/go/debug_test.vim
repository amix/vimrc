" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! Test_GoDebugStart_Empty() abort
  call s:debug()
endfunction

function! Test_GoDebugStart_RelativePackage() abort
  call s:debug('./debugmain')
endfunction

function! Test_GoDebugStart_Package() abort
  call s:debug('debugmain')
endfunction

function! s:debug(...) abort
  if !go#util#has_job()
    return
  endif

  try
    let l:tmp = gotest#load_fixture('debugmain/debugmain.go')

    call go#debug#Breakpoint(6)

    call assert_false(exists(':GoDebugStop'))

    if a:0 == 0
      let l:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
      execute l:cd . ' debugmain'
      call go#debug#Start(0)
    else
      call go#debug#Start(0, a:1)
    endif

    let l:start = reltime()
    while !exists(':GoDebugStop') && reltimefloat(reltime(l:start)) < 10
      sleep 100m
    endwhile

    call go#debug#Stop()

    call assert_false(exists(':GoDebugStop'))
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
