func! Test_ExecuteInDir() abort
  let l:tmp = gotest#write_file('a/a.go', ['package a'])
  try
    let l:out = go#tool#ExecuteInDir("pwd")
    call assert_equal(l:tmp . "/src/a\n", l:out)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_ExecuteInDir_nodir() abort
  let l:tmp = go#util#tempdir("executeindir")
  exe ':e ' . l:tmp . '/new-dir/a'

  try
    let l:out = go#tool#ExecuteInDir("pwd")
    call assert_equal('', l:out)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

" vim: sw=2 ts=2 et
