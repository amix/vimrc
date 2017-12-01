func! Test_add_tags() abort
  try
    let l:tmp = gotest#load_fixture('tags/add_all_input.go')
    silent call go#tags#run(0, 0, 40, "add", bufname(''), 1)
    call gotest#assert_fixture('tags/add_all_golden.go')
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc


func! Test_remove_tags() abort
  try
    let l:tmp = gotest#load_fixture('tags/remove_all_input.go')
    silent call go#tags#run(0, 0, 40, "remove", bufname(''), 1)
    call gotest#assert_fixture('tags/remove_all_golden.go')
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

" vim:ts=2:sts=2:sw=2:et
