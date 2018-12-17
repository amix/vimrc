" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_ExecuteInDir() abort
  let l:tmp = gotest#write_file('a/a.go', ['package a'])
  try
    let l:out = go#tool#ExecuteInDir(['pwd'])
    call assert_equal([l:tmp . "/src/a\n", 0], l:out)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_ExecuteInDir_nodir() abort
  let l:tmp = go#util#tempdir("executeindir")
  exe ':e ' . l:tmp . '/new-dir/a'

  try
    let l:out = go#tool#ExecuteInDir(['pwd'])
    call assert_equal(['', 1], l:out)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
