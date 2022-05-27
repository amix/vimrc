" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

func! Test_Callers() abort
  try
    let l:tmp = gotest#write_file('calls/caller.go', [
          \ 'package main',
          \ '',
          \ 'import "fmt"',
          \ '',
          \ 'func Quux() {}',
          \ '',
          \ 'func main() {',
          \ "\tQ\x1fuux()",
          \ "\tQuux()",
          \ '',
          \ "\tfmt.Println(\"vim-go\")",
          \ '}',
        \ ])

    let l:expected = [
          \ {'lnum': 8, 'bufnr': bufnr(''), 'col': 2, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': 'main'},
          \ {'lnum': 9, 'bufnr': bufnr(''), 'col': 2, 'pattern': '', 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'module': '', 'text': 'main'},
        \ ]

    call go#calls#Callers()

    let l:actual = getloclist(0)
    let l:start = reltime()
    while len(l:actual) != len(l:expected) && reltimefloat(reltime(l:start)) < 10
      sleep 100m
      let l:actual = getloclist(0)
    endwhile

    call gotest#assert_quickfix(l:actual, l:expected)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
