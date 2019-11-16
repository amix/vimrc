" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_GetInfo_gocode()
    let g:go_info_mode = 'gocode'
    call s:getinfo()
    unlet g:go_info_mode
endfunction

func! Test_GetInfo_guru()
    let g:go_info_mode = 'guru'
    call s:getinfo()
    unlet g:go_info_mode
endfunction

func! Test_GetInfo_gopls()
    let g:go_info_mode = 'gopls'
    call s:getinfo()
    unlet g:go_info_mode
endfunction

func! s:getinfo()
    let l:filename = 'complete/complete.go'
    let l:tmp = gotest#load_fixture(l:filename)
    try
      call cursor(8, 3)

      let expected = 'func Example(s string)'
      let actual = go#complete#GetInfo()
      call assert_equal(expected, actual)
    finally
      call delete(l:tmp, 'rf')
    endtry
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
