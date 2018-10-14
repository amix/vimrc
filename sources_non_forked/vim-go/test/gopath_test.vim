fun! Test_Detect_Gopath() abort
  let l:gopath = $GOPATH
  try
    let g:go_autodetect_gopath = 1
    let l:tmp = go#util#tempdir("pathdetect")
    let l:tmp2 = go#util#tempdir("pathdetect-nogodep")

    call mkdir(l:tmp . '/subdir', 'p')
    call mkdir(l:tmp . '/Godeps/_workspace', 'p')

    exe ':e ' . l:tmp . '/a.go'
    call assert_equal(l:tmp . '/Godeps/_workspace:' . l:gopath, $GOPATH)

    exe ':e ' . l:tmp . '/not-a-go-file'
    call assert_equal(l:gopath, $GOPATH)

    exe ':e ' . l:tmp . '/subdir/a.go'
    call assert_equal(l:tmp . '/Godeps/_workspace:' . l:gopath, $GOPATH)

    exec ':e ' . l:tmp2 . '/a.go'
    call assert_equal(l:gopath, $GOPATH)
  finally
    let g:go_autodetect_gopath = 0
    call delete(l:tmp, 'rf')
    call delete(l:tmp2, 'rf')
  endtry
endfun

fun! Test_Detect_Gopath_disabled() abort
  let l:gopath = $GOPATH
  try
    let g:go_autodetect_gopath = 0
    let l:tmp = go#util#tempdir("pathdetect")
    let l:tmp2 = go#util#tempdir("pathdetect-nogodep")

    call mkdir(l:tmp . '/subdir', 'p')
    call mkdir(l:tmp . '/Godeps/_workspace', 'p')

    exe ':e ' . l:tmp . '/a.go'
    call assert_equal(l:gopath, $GOPATH)

    exe ':e ' . l:tmp . '/not-a-go-file'
    call assert_equal(l:gopath, $GOPATH)

    exe ':e ' . l:tmp . '/subdir/a.go'
    call assert_equal(l:gopath, $GOPATH)

    exec ':e ' . l:tmp2 . '/a.go'
    call assert_equal(l:gopath, $GOPATH)
  finally
    let g:go_autodetect_gopath = 0
    call delete(l:tmp, 'rf')
    call delete(l:tmp2, 'rf')
  endtry
endfun
