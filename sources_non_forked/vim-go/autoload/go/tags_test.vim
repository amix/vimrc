" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! TestAddTags() abort
  try
    let l:tmp = gotest#load_fixture('tags/add_all_input.go')
    silent call go#tags#run(0, 0, 40, "add", bufname(''), 1)
    call gotest#assert_fixture('tags/add_all_golden.go')
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc


func! TestAddTags_WithOptions() abort
  try
    let l:tmp = gotest#load_fixture('tags/add_all_input.go')
    silent call go#tags#run(0, 0, 40, "add", bufname(''), 1, 'json,omitempty')
    call gotest#assert_fixture('tags/add_all_golden_options.go')
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! TestAddTags_AddOptions() abort
  try
    let l:tmp = gotest#load_fixture('tags/add_all_input.go')
    silent call go#tags#run(0, 0, 40, "add", bufname(''), 1, 'json')
    call gotest#assert_fixture('tags/add_all_golden.go')
    silent call go#tags#run(0, 0, 40, "add", bufname(''), 1, 'json,omitempty')
    call gotest#assert_fixture('tags/add_all_golden_options.go')
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

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim:ts=2:sts=2:sw=2:et
