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

" vim: sw=2 ts=2 et
