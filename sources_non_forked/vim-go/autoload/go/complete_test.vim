" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

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

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
