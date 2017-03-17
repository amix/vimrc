func Test_add_tags()
  let input_file = tempname()
  call writefile(readfile("test-fixtures/tags/add_all_input.go"), input_file)

  let expected = join(readfile("test-fixtures/tags/add_all_golden.go"), "\n")

  " run for offset 40, which is inside the struct
  call go#tags#run(0, 0, 40, "add", input_file, 1)

  let actual = join(readfile(input_file), "\n")

  call assert_equal(expected, actual)
endfunc


func Test_remove_tags()
  let input_file = tempname()
  call writefile(readfile("test-fixtures/tags/remove_all_input.go"), input_file)

  let expected = join(readfile("test-fixtures/tags/remove_all_golden.go"), "\n")

  " run for offset 40, which is inside the struct
  call go#tags#run(0, 0, 40, "remove", input_file, 1)

  let actual = join(readfile(input_file), "\n")

  call assert_equal(expected, actual)
endfunc
