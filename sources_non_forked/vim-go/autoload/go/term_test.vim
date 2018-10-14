func! Test_GoTermNewMode()
  if !has('nvim')
    return
  endif

  try
    let l:filename = 'term/term.go'
    let l:tmp = gotest#load_fixture(l:filename)
    exe 'cd ' . l:tmp . '/src/term'

    let expected = expand('%:p')

    let cmd = "go run ".  go#util#Shelljoin(go#tool#Files())

    set nosplitright
    call go#term#newmode(0, cmd, '')
    let actual = expand('%:p')
    call assert_equal(actual, l:expected)

  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_GoTermNewMode_SplitRight()
  if !has('nvim')
    return
  endif

  try
    let l:filename = 'term/term.go'
    let l:tmp = gotest#load_fixture(l:filename)
    exe 'cd ' . l:tmp . '/src/term'

    let expected = expand('%:p')

    let cmd = "go run ".  go#util#Shelljoin(go#tool#Files())

    set splitright
    call go#term#newmode(0, cmd, '')
    let actual = expand('%:p')
    call assert_equal(actual, l:expected)

  finally
    call delete(l:tmp, 'rf')
    set nosplitright
  endtry
endfunc

" vim: sw=2 ts=2 et
