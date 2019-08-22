" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

function! Test_GetSimpleTextPosition()
    call s:getinfo('lsp text position should align with cursor position after ascii only', 'ascii')
endfunction

function! Test_GetMultiByteTextPosition()
    call s:getinfo('lsp text position should align with cursor position after two place of interest symbols ⌘⌘', 'multi-byte')
endfunction

function! Test_GetMultipleCodeUnitTextPosition()
    call s:getinfo('lsp text position should align with cursor position after Deseret Capital Letter Long I 𐐀', 'multi-code-units')
endfunction

function! s:getinfo(str, name)
  if !go#util#has_job()
    return
  endif

  try
    let g:go_info_mode = 'gopls'

    let l:tmp = gotest#write_file(a:name . '/position/position.go', [
          \ 'package position',
          \ '',
          \ 'func Example() {',
          \ "\tid := " . '"foo"',
          \ "\tprintln(" .'"' . a:str . '", id)',
          \ '}',
          \ ] )

    let l:expected = 'var id string'
    let l:actual = go#lsp#GetInfo()
    call assert_equal(l:expected, l:actual)
  finally
    "call delete(l:tmp, 'rf')
    unlet g:go_info_mode
  endtry
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
