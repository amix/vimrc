func Test_run_fmt()
  let actual_file = tempname()
  call writefile(readfile("test-fixtures/fmt/hello.go"), actual_file)

  let expected = join(readfile("test-fixtures/fmt/hello_golden.go"), "\n")

  " run our code
  call go#fmt#run("gofmt", actual_file, "test-fixtures/fmt/hello.go")

  " this should now contain the formatted code
  let actual = join(readfile(actual_file), "\n")

  call assert_equal(expected, actual)
endfunc

func Test_update_file()
  let expected = join(readfile("test-fixtures/fmt/hello_golden.go"), "\n")
  let source_file = tempname()
  call writefile(readfile("test-fixtures/fmt/hello_golden.go"), source_file)

  let target_file = tempname()
  call writefile([""], target_file)

  " update_file now
  call go#fmt#update_file(source_file, target_file)

  " this should now contain the formatted code
  let actual = join(readfile(target_file), "\n")

  call assert_equal(expected, actual)
endfunc
