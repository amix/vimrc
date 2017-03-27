autocmd BufNewFile,BufRead *.swift set filetype=swift
autocmd BufRead * call s:Swift()
function! s:Swift()
  if !empty(&filetype)
    return
  endif

  let line = getline(1)
  if line =~ "^#!.*swift"
    setfiletype swift
  endif
endfunction
