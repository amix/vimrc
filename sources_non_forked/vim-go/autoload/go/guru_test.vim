" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function Test_GuruScope_Set() abort
  silent call go#guru#Scope("example.com/foo/bar")
  let actual = go#config#GuruScope()
  call assert_equal(["example.com/foo/bar"], actual)

  silent call go#guru#Scope('""')
  silent let actual = go#config#GuruScope()
  call assert_equal([], actual, "setting scope to empty string should clear")

  if exists('g:go_guru_scope')
    unlet g:go_guru_scope
  endif
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
