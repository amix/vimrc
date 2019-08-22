" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

function! Test_PositionOf_Simple()
  let l:actual = go#lsp#lsp#PositionOf("just ascii", 3)
  call assert_equal(4, l:actual)
endfunc


function! Test_PositionOf_MultiByte()
  " ⌘ is U+2318, which encodes to three bytes in utf-8 and 1 code unit in
  " utf-16.
  let l:actual = go#lsp#lsp#PositionOf("⌘⌘ foo", 3)
  call assert_equal(8, l:actual)
endfunc

function! Test_PositionOf_MultipleCodeUnit()
    " 𐐀 is U+10400, which encodes to 4 bytes in utf-8 and 2 code units in
    " utf-16.
    let l:actual = go#lsp#lsp#PositionOf("𐐀 bar", 3)
    call assert_equal(6, l:actual)
endfunction


" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
