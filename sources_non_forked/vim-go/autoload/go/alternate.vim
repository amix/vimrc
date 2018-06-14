" Test alternates between the implementation of code and the test code.
function! go#alternate#Switch(bang, cmd) abort
  let file = expand('%')
  if empty(file)
    call go#util#EchoError("no buffer name")
    return
  elseif file =~# '^\f\+_test\.go$'
    let l:root = split(file, '_test.go$')[0]
    let l:alt_file = l:root . ".go"
  elseif file =~# '^\f\+\.go$'
    let l:root = split(file, ".go$")[0]
    let l:alt_file = l:root . '_test.go'
  else
    call go#util#EchoError("not a go file")
    return
  endif
  if !filereadable(alt_file) && !bufexists(alt_file) && !a:bang
    call go#util#EchoError("couldn't find ".alt_file)
    return
  elseif empty(a:cmd)
    execute ":" . go#config#AlternateMode() . " " . alt_file
  else
    execute ":" . a:cmd . " " . alt_file
  endif
endfunction

" vim: sw=2 ts=2 et
