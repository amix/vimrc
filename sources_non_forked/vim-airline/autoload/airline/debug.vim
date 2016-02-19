" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

function! airline#debug#profile1()
  profile start airline-profile-switch.log
  profile func *
  profile file *
  split
  for i in range(1, 1000)
    wincmd w
    redrawstatus
  endfor
  profile pause
  noautocmd qall!
endfunction

function! airline#debug#profile2()
  profile start airline-profile-cursor.log
  profile func *
  profile file *
  edit blank
  call setline(1, 'all your base are belong to us')
  call setline(2, 'all your base are belong to us')
  let positions = [[1,2], [2,2], [1,2], [1,1]]
  for i in range(1, 1000)
    for pos in positions
      call cursor(pos[0], pos[1])
      redrawstatus
    endfor
  endfor
  profile pause
  noautocmd qall!
endfunction

function! airline#debug#profile3()
  profile start airline-profile-mode.log
  profile func *
  profile file *

  for i in range(1000)
    startinsert
    redrawstatus
    stopinsert
    redrawstatus
  endfor

  profile pause
  noautocmd qall!
endfunction

