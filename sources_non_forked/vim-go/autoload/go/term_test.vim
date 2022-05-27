" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_GoTermNewMode()
  if !(has('nvim') || has('terminal'))
    return
  endif

  try
    let l:filename = 'term/term.go'
    let l:tmp = gotest#load_fixture(l:filename)
    exe 'cd ' . l:tmp . '/src/term'

    let expected = expand('%:p')

    let cmd = "go run ".  go#util#Shelljoin(go#tool#Files())

    set nosplitright
    call go#term#new(0, cmd, &errorformat)
    let actual = expand('%:p')
    call assert_equal(actual, l:expected)

  finally
    sleep 50m
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_GoTermNewMode_SplitRight()
  if !(has('nvim') || has('terminal'))
    return
  endif

  try
    let l:filename = 'term/term.go'
    let l:tmp = gotest#load_fixture(l:filename)
    exe 'cd ' . l:tmp . '/src/term'

    let expected = expand('%:p')

    let cmd = "go run ".  go#util#Shelljoin(go#tool#Files())

    set splitright
    call go#term#new(0, cmd, &errorformat)
    let actual = expand('%:p')
    call assert_equal(actual, l:expected)

  finally
    sleep 50m
    call delete(l:tmp, 'rf')
    set nosplitright
  endtry
endfunc

func! Test_GoTermReuse()
  if !(has('nvim') || has('terminal'))
    return
  endif

  try
    let l:filename = 'term/term.go'
    let l:tmp = gotest#load_fixture(l:filename)
    exe 'cd ' . l:tmp . '/src/term'

    let expected = expand('%:p')

    let cmd = "go run ".  go#util#Shelljoin(go#tool#Files())

    set nosplitright

    let g:go_term_reuse = 1
    call go#term#new(0, cmd, &errorformat)
    let actual = expand('%:p')
    call assert_equal(actual, l:expected)
    call assert_equal(3, len(getwininfo()))

    call go#term#new(0, cmd, &errorformat)
    let actual = expand('%:p')
    call assert_equal(actual, l:expected)

    call assert_equal(3, len(getwininfo()))
  finally
    sleep 50m
    unlet g:go_term_reuse
    call delete(l:tmp, 'rf')
  endtry
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
