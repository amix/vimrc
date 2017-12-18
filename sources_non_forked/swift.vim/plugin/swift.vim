let g:tagbar_type_swift = {
  \ 'ctagstype': 'swift',
  \ 'kinds': [
    \ 'P:protocol',
    \ 'c:class',
    \ 's:struct',
    \ 'e:enum',
    \ 'E:extension',
    \ 'f:function',
    \ 't:typealias'
  \ ],
  \ 'sort': 0,
  \ 'deffile': expand('<sfile>:p:h:h') . '/ctags/swift.cnf'
\ }
