func! Test_GetInfo()
    let l:filename = 'complete/complete.go'
    let l:tmp = gotest#load_fixture(l:filename)

    call cursor(8, 3)

    let g:go_info_mode = 'gocode'
    let expected = 'func Example(s string)'
    let actual = go#complete#GetInfo()
    call assert_equal(expected, actual)

    let g:go_info_mode = 'guru'
    call go#config#InfoMode()
    let actual = go#complete#GetInfo()
    call assert_equal(expected, actual)

    unlet g:go_info_mode
endfunction

" vim: sw=2 ts=2 et
