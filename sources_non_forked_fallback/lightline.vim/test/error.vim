let s:suite = themis#suite('error')
let s:assert = themis#helper('assert')

function! s:message() abort
  redir => messages
    silent! messages
  redir END
  return split(messages, '\n')[-1]
endfunction

function! s:suite.error()
  let message = 'An error occurred.'
  call lightline#error(message)
  call s:assert.equals(s:message(), 'lightline.vim: ' . message)
endfunction
