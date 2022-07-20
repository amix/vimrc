" vim source for emails
function! coc#source#email#init() abort
  return {
        \ 'priority': 9,
        \ 'shortcut': 'Email',
        \ 'triggerCharacters': ['@']
        \}
endfunction

function! coc#source#email#should_complete(opt) abort
  return 1
endfunction

function! coc#source#email#complete(opt, cb) abort
  let items = ['foo@gmail.com', 'bar@yahoo.com']
  call a:cb(items)
endfunction
