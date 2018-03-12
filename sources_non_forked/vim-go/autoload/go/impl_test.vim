func! Test_impl() abort
  try
    let l:tmp = gotest#write_file('a/a.go', [
          \ 'package a',
          \ '',
          \ ''])

    call go#impl#Impl('r', 'reader', 'io.Reader')
    call gotest#assert_buffer(1, [
          \ 'func (r reader) Read(p []byte) (n int, err error) {',
          \ '	panic("not implemented")',
          \ '}'])
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_impl_get() abort
  try
    let l:tmp = gotest#write_file('a/a.go', [
          \ 'package a',
          \ '',
          \ 'type reader struct {}'])

    call go#impl#Impl('io.Reader')
    call gotest#assert_buffer(0, [
          \ 'package a',
          \ '',
          \ 'type reader struct {}',
          \ '',
          \ 'func (r *reader) Read(p []byte) (n int, err error) {',
          \ '	panic("not implemented")',
          \ '}'])
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc
