function! go#iferr#Generate()
  let [l:out, l:err] = go#util#Exec(['iferr',
        \ '-pos=' . go#util#OffsetCursor()], go#util#GetLines())
  if len(l:out) == 1
    return
  endif
  if getline('.') =~ '^\s*$'
    silent delete _
    silent normal! k
  endif
  let l:pos = getcurpos()
  call append(l:pos[1], split(l:out, "\n"))
  silent normal! j=2j
  call setpos('.', l:pos)
  silent normal! 4j
endfunction
