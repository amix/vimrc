" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

" New returns a promise. A promise's primary purpose is to make async jobs
" synchronous by awaiting fn.
"
" A promise is a dictionary with two keys:
"   'wrapper':
"     A function that wraps fn. It can be used in place of fn.
"   'await':
"     A function that waits for wrapper to be called and returns the value
"     returned by fn. Returns default if timeout expires.
function! go#promise#New(fn, timeout, default) abort
  let l:state = {}

  " explicitly bind to state so that within l:promise's methods, self will
  " always refer to state. See :help Partial for more information.
  return {
        \ 'wrapper': function('s:wrapper', [a:fn], l:state),
        \ 'await': function('s:await', [a:timeout, a:default], l:state),
  \ }
endfunction

function! s:wrapper(fn, ...) dict
  let self.retval = call(a:fn, a:000)
  return self.retval
endfunction

function! s:await(timeout, default) dict
  let l:timer = timer_start(a:timeout, function('s:setretval', [a:default], self))
  while !has_key(self, 'retval')
    sleep 50m
  endwhile
  call timer_stop(l:timer)

  return self.retval
endfunction

function! s:setretval(val, timer) dict
  let self.retval = a:val
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
