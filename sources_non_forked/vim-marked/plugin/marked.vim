" marked.vim
" Author:  Joshua Priddle <jpriddle@me.com>
" URL:     https://github.com/itspriddle/vim-marked
" Version: 0.4.0
" License: Same as Vim itself (see :help license)

if &cp || exists("g:marked_loaded") && g:marked_loaded
  finish
endif
let g:marked_loaded = 1
let s:save_cpo = &cpo
set cpo&vim

function s:OpenMarked(background)
  let l:filename = expand("%:p")
  silent exe "!open -a Marked.app ".(a:background ? '-g' : '')." '".l:filename."'"

  silent exe "augroup marked_autoclose_".l:filename
    autocmd!
    silent exe 'autocmd VimLeavePre * call s:QuitMarked("'.l:filename.'")'
  augroup END
  redraw!
endfunction

function s:QuitMarked(path)
  silent exe "augroup marked_autoclose_".a:path
    autocmd!
  augroup END
  silent exe "augroup! marked_autoclose_".a:path

  let cmd  = " -e 'try'"
  let cmd .= " -e 'if application \"Marked\" is running then'"
  let cmd .= " -e 'tell application \"Marked\"'"
  let cmd .= " -e 'close (first document whose path is equal to \"".a:path."\")'"
  let cmd .= " -e 'if count of every window is equal to 0 then'"
  let cmd .= " -e 'quit'"
  let cmd .= " -e 'end if'"
  let cmd .= " -e 'end tell'"
  let cmd .= " -e 'end if'"
  let cmd .= " -e 'end try'"

  silent exe "!osascript ".cmd
  redraw!
endfunction

augroup marked_commands
  autocmd!
  autocmd FileType markdown,mkd command! -buffer -bang MarkedOpen :call s:OpenMarked(<bang>0)
  autocmd FileType markdown,mkd command! -buffer MarkedQuit :call s:QuitMarked(expand('%:p'))
augroup END

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:ft=vim:fdm=marker:ts=2:sw=2:sts=2:et
