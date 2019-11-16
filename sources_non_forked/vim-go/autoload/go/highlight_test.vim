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
        \ 'hexadecimal': {'group': 'goHexadecimalInt', 'value': '0x0123456789abdef'},
        \ 'hexadecimalErrorLeading': {'group': 'goHexadecimalError', 'value': '0xg0123456789abdef'},
        \ 'hexadecimalErrorTrailing': {'group': 'goHexadecimalError', 'value': '0x0123456789abdefg'},
        \ 'heXadecimal': {'group': 'goHexadecimalInt', 'value': '0X0123456789abdef'},
        \ 'heXadecimalErrorLeading': {'group': 'goHexadecimalError', 'value': '0Xg0123456789abdef'},
        \ 'heXadecimalErrorTrailing': {'group': 'goHexadecimalError', 'value': '0X0123456789abdefg'},
        \ 'octal': {'group': 'goOctalInt', 'value': '01234567'},
        \ 'octalErrorLeading': {'group': 'goOctalError', 'value': '081234567'},
        \ 'octalErrorTrailing': {'group': 'goOctalError', 'value': '012345678'},
        \ 'binaryInt': {'group': 'goBinaryInt', 'value': '0b0101'},
        \ 'binaryErrorLeading': {'group': 'goBinaryError', 'value': '0b20101'},
        \ 'binaryErrorTrailing': {'group': 'goBinaryError', 'value': '0b01012'},
        \ 'BinaryInt': {'group': 'goBinaryInt', 'value': '0B0101'},
        \ 'BinaryErrorLeading': {'group': 'goBinaryError', 'value': '0B20101'},
        \ 'BinaryErrorTrailing': {'group': 'goBinaryError', 'value': '0B01012'},
        \ }

  for kv in items(tests)
    let l:dir = gotest#write_file(printf('numeric/%s.go', kv[0]), [
          \ 'package numeric',
          \ '',
          \ printf("var v = %s\x1f", kv[1].value),
          \ ])

    try
      let l:pos = getcurpos()
      let l:actual = synIDattr(synID(l:pos[1], l:pos[2], 1), 'name')
      call assert_equal(kv[1].group, l:actual, kv[0])
    finally
      " call delete(l:dir, 'rf')
    endtry
  endfor
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
