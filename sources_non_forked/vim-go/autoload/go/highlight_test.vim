" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! Test_gomodVersion_highlight() abort
  try
    syntax on

    let l:dir = gotest#write_file('gomodtest/go.mod', [
          \ 'module github.com/fatih/vim-go',
          \ '',
          \ '\x1frequire (',
          \ '\tversion/simple v1.0.0',
          \ '\tversion/simple-pre-release v1.0.0-rc',
          \ '\tversion/simple-pre-release v1.0.0+meta',
          \ '\tversion/simple-pre-release v1.0.0-rc+meta',
          \ '\tversion/pseudo/premajor v1.0.0-20060102150405-0123456789abcdef',
          \ '\tversion/pseudo/prerelease v1.0.0-prerelease.0.20060102150405-0123456789abcdef',
          \ '\tversion/pseudo/prepatch v1.0.1-0.20060102150405-0123456789abcdef',
          \ '\tversion/simple/incompatible v2.0.0+incompatible',
          \ '\tversion/pseudo/premajor/incompatible v2.0.0-20060102150405-0123456789abcdef+incompatible',
          \ '\tversion/pseudo/prerelease/incompatible v2.0.0-prerelease.0.20060102150405-0123456789abcdef+incompatible',
          \ '\tversion/pseudo/prepatch/incompatible v2.0.1-0.20060102150405-0123456789abcdef+incompatible',
          \ ')'])

    let l:lineno = 4
    let l:lineclose = line('$')
    while l:lineno < l:lineclose
      let l:line = getline(l:lineno)
      let l:col = col([l:lineno, '$']) - 1
      let l:idx = len(l:line) - 1
      let l:from = stridx(l:line, ' ') + 1

      while l:idx >= l:from
        call cursor(l:lineno, l:col)
        let l:synname = synIDattr(synID(l:lineno, l:col, 1), 'name')
        let l:errlen = len(v:errors)

        call assert_equal('gomodVersion', l:synname, 'version on line ' . l:lineno)

        " continue at the next line if there was an error at this column;
        " there's no need to test each column once an error is detected.
        if l:errlen < len(v:errors)
          break
        endif

        let l:col -= 1
        let l:idx -= 1
      endwhile
      let l:lineno += 1
    endwhile
  finally
    call delete(l:dir, 'rf')
  endtry
endfunc

function! Test_gomodVersion_incompatible_highlight() abort
  try
    syntax on

    let l:dir = gotest#write_file('gomodtest/go.mod', [
          \ 'module github.com/fatih/vim-go',
          \ '',
          \ '\x1frequire (',
          \ '\tversion/invalid/premajor/incompatible v1.0.0-20060102150405-0123456789abcdef+incompatible',
          \ '\tversion/invalid/prerelease/incompatible v1.0.0-prerelease.0.20060102150405-0123456789abcdef+incompatible',
          \ '\tversion/invalid/prepatch/incompatible v1.0.1-0.20060102150405-0123456789abcdef+incompatible',
          \ ')'])

    let l:lineno = 4
    let l:lineclose = line('$')
    while l:lineno < l:lineclose
      let l:line = getline(l:lineno)
      let l:col = col([l:lineno, '$']) - 1
      let l:idx = len(l:line) - 1
      let l:from = stridx(l:line, '+')

      while l:idx >= l:from
        call cursor(l:lineno, l:col)
        let l:synname = synIDattr(synID(l:lineno, l:col, 1), 'name')
        let l:errlen = len(v:errors)

        call assert_notequal('gomodVersion', l:synname, 'version on line ' . l:lineno)

        " continue at the next line if there was an error at this column;
        " there's no need to test each column once an error is detected.
        if l:errlen < len(v:errors)
          break
        endif

        let l:col -= 1
        let l:idx -= 1
      endwhile
      let l:lineno += 1
    endwhile
  finally
    call delete(l:dir, 'rf')
  endtry
endfunc

function! Test_numeric_literal_highlight() abort
  syntax on

  let tests = {
        \ 'lone zero': {'group': 'goDecimalInt', 'value': '0'},
        \ 'integer': {'group': 'goDecimalInt', 'value': '1234567890'},
        \ 'integerGrouped': {'group': 'goDecimalInt', 'value': '1_234_567_890'},
        \ 'hexadecimal': {'group': 'goHexadecimalInt', 'value': '0x0123456789abdef'},
        \ 'hexadecimalGrouped': {'group': 'goHexadecimalInt', 'value': '0x012_345_678_9ab_def'},
        \ 'heXadecimal': {'group': 'goHexadecimalInt', 'value': '0X0123456789abdef'},
        \ 'octal': {'group': 'goOctalInt', 'value': '01234567'},
        \ 'octalPrefix': {'group': 'goOctalInt', 'value': '0o1234567'},
        \ 'octalGrouped': {'group': 'goOctalInt', 'value': '0o1_234_567'},
        \ 'OctalPrefix': {'group': 'goOctalInt', 'value': '0O1234567'},
        \ 'binaryInt': {'group': 'goBinaryInt', 'value': '0b0101'},
        \ 'binaryIntGrouped': {'group': 'goBinaryInt', 'value': '0b_01_01'},
        \ 'BinaryInt': {'group': 'goBinaryInt', 'value': '0B0101'},
        \ }

  for kv in items(tests)
    let l:actual = s:numericHighlightGroupInAssignment(kv[0], kv[1].value)
    call assert_equal(kv[1].group, l:actual, kv[0])
  endfor
endfunction

function! Test_zero_as_index_element() abort
  syntax on

  let l:actual = s:numericHighlightGroupInSliceElement('zero-element', '0')
  call assert_equal('goDecimalInt', l:actual)
  let l:actual = s:numericHighlightGroupInMultidimensionalSliceElement('zero-element', '0')
  call assert_equal('goDecimalInt', l:actual, 'multi-dimensional')
endfunction

function! Test_zero_as_slice_index() abort
  syntax on

  let l:actual = s:numericHighlightGroupInSliceIndex('zero-index', '0')
  call assert_equal('goDecimalInt', l:actual)
  let l:actual = s:numericHighlightGroupInMultidimensionalSliceIndex('zero-index', '0', '0')

  call assert_equal('goDecimalInt', l:actual, 'multi-dimensional')
endfunction

function! Test_zero_as_start_slicing_slice() abort
  syntax on

  let l:actual = s:numericHighlightGroupInSliceSlicing('slice-slicing', '0', '1')
  call assert_equal('goDecimalInt', l:actual)
endfunction

function! s:numericHighlightGroupInAssignment(testname, value)
  let l:dir = gotest#write_file(printf('numeric/%s.go', a:testname), [
        \ 'package numeric',
        \ '',
        \ printf("var v = %s\x1f", a:value),
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    return l:actual
  finally
    call delete(l:dir, 'rf')
  endtry
endfunction

function! s:numericHighlightGroupInSliceElement(testname, value)
  let l:dir = gotest#write_file(printf('numeric/slice-element/%s.go', a:testname), [
        \ 'package numeric',
        \ '',
        \ printf("v := []int{%s\x1f}", a:value),
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    return l:actual
  finally
    call delete(l:dir, 'rf')
  endtry
endfunction

function! s:numericHighlightGroupInMultidimensionalSliceElement(testname, value)
  let l:dir = gotest#write_file(printf('numeric/slice-multidimensional-element/%s.go', a:testname), [
        \ 'package numeric',
        \ '',
        \ printf("v := [][]int{{%s\x1f},{%s}}", a:value, a:value),
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    return l:actual
  finally
    call delete(l:dir, 'rf')
  endtry
endfunction

function! s:numericHighlightGroupInSliceIndex(testname, value)
  let l:dir = gotest#write_file(printf('numeric/slice-index/%s.go', a:testname), [
        \ 'package numeric',
        \ '',
        \ 'var sl []int',
        \ printf("println(sl[%s\x1f])", a:value),
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    return l:actual
  finally
    call delete(l:dir, 'rf')
  endtry
endfunction

function! s:numericHighlightGroupInMultidimensionalSliceIndex(testname, first, second)
  let l:dir = gotest#write_file(printf('numeric/slice-multidimensional-index/%s.go', a:testname), [
        \ 'package numeric',
        \ '',
        \ 'var sl [][]int',
        \ printf("println(sl[%s\x1f][%s])", a:first, a:second),
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    return l:actual
  finally
    call delete(l:dir, 'rf')
  endtry
endfunction

function! s:numericHighlightGroupInSliceSlicing(testname, from, to)
  let l:dir = gotest#write_file(printf('numeric/slice-slicing/%s.go', a:testname), [
        \ 'package numeric',
        \ '',
        \ 'var sl = []int{1,2}',
        \ printf("println(sl[%s\x1f:%s])", a:from, a:to),
        \ ])
  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    return l:actual
  finally
    call delete(l:dir, 'rf')
  endtry
endfunction

function! Test_diagnostic_after_fmt() abort
  let g:go_fmt_command = 'gofmt'
  let g:go_diagnostics_level = 2
  try
    call s:diagnostic_after_write( [
          \ 'package main',
          \ 'import "fmt"',
          \ '',
          \ 'func main() {',
          \ '',
          \ "\tfmt.Println(h\x1fello)",
          \ '}',
          \ ], [])
  finally
    unlet g:go_fmt_command
  endtry
endfunction

function! Test_diagnostic_after_fmt_change() abort
  " craft a file that will be changed when its written (gofmt will change it).
  let g:go_fmt_command = 'gofmt'
  let g:go_diagnostics_level = 2
  try
    call s:diagnostic_after_write( [
          \ 'package main',
          \ 'import "fmt"',
          \ '',
          \ 'func main() {',
          \ '',
          \ "fmt.Println(h\x1fello)",
          \ '}',
          \ ], [])
  finally
    unlet g:go_fmt_command
  endtry
endfunction

function! Test_diagnostic_after_fmt_cleared() abort
  " craft a file that will be fixed when it is written.
  let g:go_fmt_command = 'gofmt'
  let g:go_diagnostics_level = 2
  try
    call s:diagnostic_after_write( [
          \ 'package main',
          \ 'import "fmt"',
          \ '',
          \ 'func main() {',
          \ '',
          \ "fmt.Println(h\x1fello)",
          \ '}',
          \ ], ['hello := "hello, vim-go"'])
  finally
    unlet g:go_fmt_command
  endtry
endfunction

function! Test_diagnostic_after_reload() abort
  let g:go_diagnostics_level = 2
  let l:dir = gotest#write_file('diagnostic/after-reload.go', [
              \ 'package main',
              \ 'import "fmt"',
              \ '',
              \ 'func main() {',
              \ '',
              \ "\tfmt.Println(h\x1fello)",
              \ '}',
              \ ])
  try
    call s:check_diagnostics('', 'goDiagnosticError', 'initial')
    let l:pos = getcurpos()
    edit
    call setpos('.', l:pos)
    call s:check_diagnostics('', 'goDiagnosticError', 'after-reload')
  finally
    call delete(l:dir, 'rf')
  endtry
endfunction

function! s:diagnostic_after_write(contents, changes) abort
  syntax on

  let g:go_diagnostics_level = 2
  let l:dir = gotest#write_file('diagnostic/after-write.go', a:contents)

  try
    let l:pos = getcurpos()
    call s:check_diagnostics('', 'goDiagnosticError', 'initial')

    " write a:changes to the previous line and make sure l:actual and
    " l:expected are set so that they won't accidentally match on the next
    " check.
    if len(a:changes) > 0
      call append(l:pos[1]-1, a:changes)
      let l:actual = 'goDiagnosticError'
      let l:expected = ''
    else
      let l:actual = ''
      let l:expected = 'goDiagnosticError'
    endif

    write

    call s:check_diagnostics(l:actual, l:expected, 'after-write')
  finally
    call delete(l:dir, 'rf')
  endtry
endfunction

function! s:check_diagnostics(actual, expected, when)
  let l:actual = a:actual
  let l:start = reltime()

  while l:actual != a:expected && reltimefloat(reltime(l:start)) < 10
    " Get the cursor position on each iteration, because the cursor postion
    " may change between iterations when go#fmt#GoFmt formats, reloads the
    " file, and moves the cursor to try to keep it where the user expects it
    " to be when gofmt modifies the files.
    let l:pos = getcurpos()
    if !has('textprop')
      let l:matches = getmatches()
      if len(l:matches) == 0
        let l:actual = ''
      endif

      for l:m in l:matches
        let l:matchline = l:m.pos1[0]
        if len(l:m.pos1) < 2
          continue
        endif
        let l:matchcol = get(l:m.pos1, 1, 1)
        if l:pos[1] == l:matchline && l:pos[2] >= l:matchcol && l:pos[2] <= l:matchcol + l:m.pos1[2]
        " Ideally, we'd check that the cursor is within the match, but when a
        " tab is added on the current line, the cursor position within the
        " line will stay constant while the line itself is shifted over by a
        " column, so just check the line itself instead of checking a precise
        " cursor location.
        " if l:pos[1] == l:matchline
          let l:actual = l:m.group
          break
        endif
      endfor

      sleep 100m
      continue
    endif

    let l:actual = get(prop_list(l:pos[1]), 0, {'type': ''}).type
    sleep 100m
  endwhile

  call assert_equal(a:expected, l:actual, a:when)
endfunction

function! Test_goStringHighlight() abort
  syntax on

  let l:dir = gotest#write_file('highlight/gostring.go', [
        \ 'package highlight',
        \ '',
        \ 'import (',
        \ printf("\t%s", '"fmt"'),
        \ ')',
        \ '',
        \ printf('var s = "%s"', "gostring\x1f"),
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    call assert_equal('goString', l:actual)
  finally
    call delete(l:dir, 'rf')
  endtry
endfunc

function! Test_goImportStringHighlight() abort
  syntax on

  let l:dir = gotest#write_file('highlight/import.go', [
        \ 'package highlight',
        \ '',
        \ 'import (',
        \ printf('%s"%s"', "\t", "f\x1fmt"),
        \ ')',
        \ '',
        \ 'var s = fmt.Sprint("gostring")',
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    call assert_equal('goImportString', l:actual)
  finally
    call delete(l:dir, 'rf')
  endtry
endfunc

function! Test_goReceiverHighlight() abort
  syntax on

  let l:tests = {
      \ 'PointerReceiverVar': {'group': 'goReceiverVar', 'value': "t\x1f *T"},
      \ 'ValueReceiverVar': {'group': 'goReceiverVar', 'value': "t\x1f T"},
      \ 'PointerReceiverType': {'group': 'goReceiverType', 'value': "t *T\x1f"},
      \ 'ValueReceiverType': {'group': 'goReceiverType', 'value': "t T\x1f"},
      \ 'PointerReceiverTypeOmittedVar': {'group': 'goReceiverType', 'value': "*T\x1f"},
      \ 'ValueReceiverTypeOmittedVar': {'group': 'goReceiverType', 'value': "T\x1f"},
      \ 'GenericPointerReceiverVar': {'group': 'goReceiverVar', 'value': "g\x1f *G[int]"},
      \ 'GenericValueReceiverVar': {'group': 'goReceiverVar', 'value': "g\x1f G[int]"},
      \ 'GenericPointerReceiverType': {'group': 'goReceiverType', 'value': "g *G\x1f[int]"},
      \ 'GenericValueReceiverType': {'group': 'goReceiverType', 'value': "g G\x1f[int]"},
      \ 'GenericPointerReceiverTypeOmittedVar': {'group': 'goReceiverType', 'value': "*G\x1f[int]"},
      \ 'GenericValueReceiverTypeOmittedVar': {'group': 'goReceiverType', 'value': "G\x1f[int]"},
      \ }

  let g:go_highlight_function_parameters = 1
  for l:kv in items(l:tests)
    let l:actual = s:receiverHighlightGroup(l:kv[0], l:kv[1].value)
    call assert_equal(l:kv[1].group, l:actual, l:kv[0])
  endfor
  unlet g:go_highlight_function_parameters
endfunc

function! s:receiverHighlightGroup(testname, value)
  let l:package = tolower(a:testname)
  let l:dir = gotest#write_file(printf('%s/%s.go', l:package, a:testname), [
        \ printf('package %s', l:package),
        \ '',
        \ 'type T struct{}',
        \ 'type G[T any] struct{}',
        \ printf('func (%s) Foo() {}', a:value),
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    return l:actual
  finally
    call delete(l:dir, 'rf')
  endtry
endfunc

function! Test_GoTypeHighlight() abort
  syntax on

  let l:tests = {
      \ 'StandardType': {'group': 'goTypeName', 'value': "T\x1f"},
      \ 'GenericType': {'group': 'goTypeName', 'value': "G\x1f[T any]"},
      \ }

  let g:go_highlight_types = 1
  for l:kv in items(l:tests)
    let l:actual = s:typeHighlightGroup(l:kv[0], l:kv[1].value)
    call assert_equal(l:kv[1].group, l:actual, l:kv[0])
  endfor
  unlet g:go_highlight_types
endfunc

function! s:typeHighlightGroup(testname, value)
  let l:package = tolower(a:testname)
  let l:dir = gotest#write_file(printf('%s/%s.go', l:package, a:testname), [
        \ printf('package %s', l:package),
        \ '',
        \ printf('type %s struct{}', a:value),
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    return l:actual
  finally
    call delete(l:dir, 'rf')
  endtry
endfunc

function! Test_goFunction() abort
  syntax on

  let l:tests = {
        \ 'StandardFunction': {'group': 'goFunction', 'value': "F\x1f(){}"},
        \ 'GenericFunction': {'group': 'goFunction', 'value': "G\x1f[T any](_ T){}"},
      \ }

  let g:go_highlight_functions = 1
  for l:kv in items(l:tests)
    let l:actual = s:functionHighlightGroup(l:kv[0], l:kv[1].value)
    call assert_equal(l:kv[1].group, l:actual, l:kv[0])
  endfor
  unlet g:go_highlight_functions
endfunc

function! s:functionHighlightGroup(testname, value)
  let l:package = tolower(a:testname)
  let l:dir = gotest#write_file(printf('%s/%s.go', l:package, a:testname), [
        \ printf('package %s', l:package),
        \ '',
        \ printf('func %s', a:value),
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    return l:actual
  finally
    call delete(l:dir, 'rf')
  endtry
endfunc

function! Test_goFunctionCall() abort
  syntax on

  let l:tests = {
      \ 'StandardFunctionCall': {'group': 'goFunctionCall', 'value': "f\x1f()"},
      \ 'GenericFunctionCall': {'group': 'goFunctionCall', 'value': "g\x1f[int](i)"},
      \ }

  let g:go_highlight_function_calls = 1
  for l:kv in items(l:tests)
    let l:actual = s:functionCallHighlightGroup(l:kv[0], l:kv[1].value)
    call assert_equal(l:kv[1].group, l:actual, l:kv[0])
  endfor
  unlet g:go_highlight_function_calls
endfunc

function! s:functionCallHighlightGroup(testname, value)
  let l:package = tolower(a:testname)
  let l:dir = gotest#write_file(printf('%s/%s.go', l:package, a:testname), [
        \ printf('package %s', l:package),
        \ '',
        \ 'func f() {}',
        \ 'func g[T any](i T) {}',
        \ 'func init() {',
        \ printf("\t%s", a:value),
        \ '}',
        \ ])

  try
    let l:pos = getcurpos()
    let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
    return l:actual
  finally
    call delete(l:dir, 'rf')
  endtry
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
