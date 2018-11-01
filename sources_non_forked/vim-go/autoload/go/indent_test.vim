func! Test_indent_raw_string() abort
  " The goRawString discovery requires that syntax be enabled.
  syntax on

  try
    let l:dir= gotest#write_file('indent/indent.go', [
          \ 'package main',
          \ '',
          \ 'import "fmt"',
          \ '',
          \ 'func main() {',
          \	"\t\x1fconst msg = `",
          \ '`',
          \ '\tfmt.Println(msg)',
          \ '}'])

    silent execute "normal o" . "not indented\<Esc>"
    let l:indent = indent(line('.'))
    call assert_equal(0, l:indent)
  finally
    call delete(l:dir, 'rf')
  endtry

  try
    let l:dir= gotest#write_file('indent/indent.go', [
          \ 'package main',
          \ '',
          \ 'import "fmt"',
          \ '',
          \ 'func main() {',
          \	"\t\x1fmsg := `",
          \ '`',
          \ '\tfmt.Println(msg)',
          \ '}'])

    silent execute "normal o" . "not indented\<Esc>"
    let l:indent = indent(line('.'))
    call assert_equal(0, l:indent)
  finally
    call delete(l:dir, 'rf')
  endtry

  try
    let l:dir= gotest#write_file('indent/indent.go', [
          \ 'package main',
          \ '',
          \ 'import "fmt"',
          \ '',
          \ 'func main() {',
          \	"\tconst msg = `",
          \ "\t\x1findented",
          \ '`',
          \ '\tfmt.Println(msg)',
          \ '}'])

    silent execute "normal o" . "indented\<Esc>"
    let l:indent = indent(line('.'))
    call assert_equal(shiftwidth(), l:indent)
  finally
    call delete(l:dir, 'rf')
  endtry
endfunc
