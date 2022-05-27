" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

func! Test_EncodePath_simple() abort
	let l:uri = '/simple/foo'
	let l:expected = '/simple/foo'

	let l:actual = go#uri#EncodePath(l:uri)
	call assert_equal(l:expected, l:actual)
endfunc

func! Test_EncodePath_multibyte() abort
  let l:uri = '/multi-byte/⌘⌘'
  let l:expected = '/multi-byte/%E2%8C%98%E2%8C%98'

	let l:actual = go#uri#EncodePath(l:uri)
	call assert_equal(l:expected, l:actual)
endfunc

func! Test_Decode_simple() abort
	let l:uri = '/simple/foo'
	let l:expected = '/simple/foo'

	let l:actual = go#uri#Decode(l:uri)
	call assert_equal(l:expected, l:actual)
endfunc

func! Test_Decode_multibyte() abort
  let l:uri = '/multi-byte/%E2%8C%98%E2%8C%98'
  let l:expected = '/multi-byte/⌘⌘'
	let l:actual = go#uri#Decode(l:uri)
	call assert_equal(l:expected, l:actual)
endfunc

func! Test_Roundtrip_simple() abort
	let l:expected = '/simple/foo'

	let l:actual = go#uri#Decode(go#uri#EncodePath(l:expected))
	call assert_equal(l:expected, l:actual)
endfunc

func! Test_Roundtrip_multibyte() abort
  let l:expected = '/multi-byte/⌘⌘'

	let l:actual = go#uri#Decode(go#uri#EncodePath(l:expected))
  call assert_equal(l:expected, l:actual)
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
