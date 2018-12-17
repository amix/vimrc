" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_jump_to_declaration_guru() abort
  try
    let l:filename = 'def/jump.go'
    let lnum = 5
    let col = 6
    let l:tmp = gotest#load_fixture(l:filename)

    let guru_out = printf("%s:%d:%d: defined here as func main", filename, lnum, col)
    call go#def#jump_to_declaration(guru_out, "", 'guru')

    call assert_equal(filename, bufname("%"))
    call assert_equal(lnum, getcurpos()[1])
    call assert_equal(col, getcurpos()[2])
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_jump_to_declaration_godef() abort
  try
    let filename = 'def/jump.go'
    let lnum = 5
    let col = 6
    let l:tmp = gotest#load_fixture(l:filename)

    let godef_out = printf("%s:%d:%d\ndefined here as func main", filename, lnum, col)
    call go#def#jump_to_declaration(godef_out, "", 'godef')

    call assert_equal(filename, bufname("%"))
    call assert_equal(lnum, getcurpos()[1])
    call assert_equal(col, getcurpos()[2])
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

func! Test_Jump_leaves_lists() abort
  try
    let filename = 'def/jump.go'
    let l:tmp = gotest#load_fixture(l:filename)

    let expected = [{'lnum': 10, 'bufnr': bufnr('%'), 'col': 1, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'text': 'quux'}]

    call setloclist(winnr(), copy(expected), 'r' )
    call setqflist(copy(expected), 'r' )

    let l:bufnr = bufnr('%')
    call cursor(6, 7)
    call go#def#Jump('')

    let start = reltime()
    while bufnr('%') == l:bufnr && reltimefloat(reltime(start)) < 10
      sleep 100m
    endwhile

    let actual = getloclist(winnr())
    call gotest#assert_quickfix(actual, expected)

    let actual = getqflist()
    call gotest#assert_quickfix(actual, expected)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
