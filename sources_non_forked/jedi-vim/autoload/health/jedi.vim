function! health#jedi#check() abort
  call health#report_start('jedi')
  silent call jedi#debug_info()
endfunction
