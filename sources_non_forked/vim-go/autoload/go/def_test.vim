func Test_jump_to_declaration_guru()
  let file_name = "test-fixtures/def/jump.go"
  let lnum = 5
  let col = 6

  let out = printf("%s:%d:%d: defined here as func main", file_name, lnum, col)
  let bin_name = "guru"

  call go#def#jump_to_declaration(out, "", bin_name)

  call assert_equal(file_name, bufname("%"))
  call assert_equal(lnum, getcurpos()[1])
  call assert_equal(col, getcurpos()[2])
endfunc

func Test_jump_to_declaration_godef()
  let file_name = "test-fixtures/def/jump.go"
  let lnum = 5
  let col = 6

  " note that the output of godef has two lines
  let out = printf("%s:%d:%d\ndefined here as func main", file_name, lnum, col)
  let bin_name = "godef"

  call go#def#jump_to_declaration(out, "", bin_name)

  call assert_equal(file_name, bufname("%"))
  call assert_equal(lnum, getcurpos()[1])
  call assert_equal(col, getcurpos()[2])
endfunc

" vim: sw=2 ts=2 et
