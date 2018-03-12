func! Test_fillstruct() abort
  try
    let l:tmp = gotest#write_file('a/a.go', [
          \ 'package a',
          \ 'import "net/mail"',
          \ "var addr = mail.\x1fAddress{}"])

    call go#fillstruct#FillStruct()
    call gotest#assert_buffer(1, [
          \ 'var addr = mail.Address{',
          \ '\tName:    "",',
          \ '\tAddress: "",',
          \ '}'])
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_fillstruct_line() abort
  try
    let l:tmp = gotest#write_file('a/a.go', [
          \ 'package a',
          \ 'import "net/mail"',
          \ "\x1f" . 'var addr = mail.Address{}'])

    call go#fillstruct#FillStruct()
    call gotest#assert_buffer(1, [
          \ 'var addr = mail.Address{',
          \ '\tName:    "",',
          \ '\tAddress: "",',
          \ '}'])
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_fillstruct_two_line() abort
  try
    let l:tmp = gotest#write_file('a/a.go', [
          \ 'package a',
          \ 'import (',
          \ '"fmt"',
          \ '"net/mail"',
          \ ')',
          \ "\x1f" . 'func x() { fmt.Println(mail.Address{}, mail.Address{}) }'])

    call go#fillstruct#FillStruct()
    call gotest#assert_buffer(1, [
          \ 'import (',
          \ '"fmt"',
          \ '"net/mail"',
          \ ')',
          \ 'func x() { fmt.Println(mail.Address{',
          \ '\tName:    "",',
          \ '\tAddress: "",',
          \ '}, mail.Address{',
          \ '\tName:    "",',
          \ '\tAddress: "",',
          \ '}) }'])
  finally
    "call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_fillstruct_two_cursor() abort
  try
    let l:tmp = gotest#write_file('a/a.go', [
          \ 'package a',
          \ 'import (',
          \ '"fmt"',
          \ '"net/mail"',
          \ ')',
          \ "func x() { fmt.Println(mail.Address{}, mail.Ad\x1fdress{}) }"])

    call go#fillstruct#FillStruct()
    call gotest#assert_buffer(1, [
          \ 'import (',
          \ '"fmt"',
          \ '"net/mail"',
          \ ')',
          \ 'func x() { fmt.Println(mail.Address{}, mail.Address{',
          \ '\tName:    "",',
          \ '\tAddress: "",',
          \ '}) }'])
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

" vim: sw=2 ts=2 et
