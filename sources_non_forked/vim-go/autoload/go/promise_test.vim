" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_PromiseNew() abort
	let l:sut = go#promise#New(function('s:work', []), 100, -1)
	call assert_true(has_key(l:sut, 'wrapper'))
	call assert_true(has_key(l:sut, 'await'))
endfunc

func! Test_PromiseAwait() abort
	let l:expected = 1
	let l:default = -1
	let l:sut = go#promise#New(function('s:work', [l:expected]), 100, l:default)

	call timer_start(10, l:sut.wrapper)

	let l:actual = call(l:sut.await, [])
	call assert_equal(l:expected, l:actual)
endfunc

func! Test_PromiseAwait_Timeout() abort
	let l:desired = 1
	let l:expected = -1
	let l:sut = go#promise#New(function('s:work', [l:desired]), 10, l:expected)

	call timer_start(100, l:sut.wrapper)

	let l:actual = call(l:sut.await, [])
	call assert_equal(l:expected, l:actual)
endfunc

func! s:work(val, timer)
	return a:val
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
