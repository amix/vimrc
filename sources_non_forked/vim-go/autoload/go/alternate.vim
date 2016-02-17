" By default use edit (current buffer view) to switch
if !exists("g:go_alternate_mode")
  let g:go_alternate_mode = "edit"
endif

" Test alternates between the implementation of code and the test code.
function! go#alternate#Switch(bang, cmd)
  let l:file = go#alternate#Filename(fnameescape(expand("%")))
  if !filereadable(l:file) && !bufexists(l:file) && !a:bang
    redraws! | echon "vim-go: " | echohl ErrorMsg | echon "couldn't find ".file | echohl None
    return
  elseif empty(a:cmd)
    execute ":" . g:go_alternate_mode . " " . file
  else
    execute ":" . a:cmd . " " . file
  endif
endfunction

" Filename returns the name of the test file or implementation file
" depending on the arguments
function! go#alternate#Filename(path)
  if empty(matchstr(a:path, "_test"))
    let l:root = split(a:path, ".go$")[0]
    let l:file = l:root . "_test.go"
  else
    let l:root = split(a:path, "_test.go$")[0]
    let l:file = l:root . ".go"
  endif
  return l:file
endfunction
