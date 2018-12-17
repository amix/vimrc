" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

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

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
