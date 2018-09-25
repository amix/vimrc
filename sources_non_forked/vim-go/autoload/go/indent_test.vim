func! Test_indent_raw_string() abort
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
endfunc

" vim: sw=2 ts=2 et
