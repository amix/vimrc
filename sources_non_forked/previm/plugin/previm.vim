" AUTHOR: kanno <akapanna@gmail.com>
" License: This file is placed in the public domain.

if exists('g:loaded_previm') && g:loaded_previm
  finish
endif
let g:loaded_previm = 1

let s:save_cpo = &cpo
set cpo&vim

function! s:setup_setting()
  augroup Previm
    autocmd! * <buffer>
    if get(g:, "previm_enable_realtime", 0) ==# 1
      " NOTE: It is too frequently in TextChanged/TextChangedI
      autocmd CursorHold,CursorHoldI,InsertLeave,BufWritePost <buffer> call previm#refresh()
    else
      autocmd BufWritePost <buffer> call previm#refresh()
    endif
  augroup END

  command! -buffer -nargs=0 PrevimOpen call previm#open(previm#make_preview_file_path('index.html'))
endfunction

augroup Previm
  autocmd!
  autocmd FileType *{mkd,markdown,rst,textile}* call <SID>setup_setting()
augroup END

let &cpo = s:save_cpo
unlet! s:save_cpo
