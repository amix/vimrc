" Vim syntastic plugin helper
" Language:     Rust
" Maintainer:   Andrew Gallant <jamslam@gmail.com>

if exists("g:loaded_syntastic_rust_filetype")
  finish
endif
let g:loaded_syntastic_rust_filetype = 1
let s:save_cpo = &cpo
set cpo&vim

" This is to let Syntastic know about the Rust filetype.
" It enables tab completion for the 'SyntasticInfo' command.
" (This does not actually register the syntax checker.)
if exists('g:syntastic_extra_filetypes')
    call add(g:syntastic_extra_filetypes, 'rust')
else
    let g:syntastic_extra_filetypes = ['rust']
endif

let &cpo = s:save_cpo
unlet s:save_cpo

command! -nargs=* Cargo call cargo#cmd(<q-args>)
command! -nargs=* Cbuild call cargo#build(<q-args>)
command! -nargs=* Cclean call cargo#clean(<q-args>)
command! -nargs=* Cdoc call cargo#doc(<q-args>)
command! -nargs=* Cnew call cargo#new(<q-args>)
command! -nargs=* Cinit call cargo#init(<q-args>)
command! -nargs=* Crun call cargo#run(<q-args>)
command! -nargs=* Ctest call cargo#test(<q-args>)
command! -nargs=* Cbench call cargo#bench(<q-args>)
command! -nargs=* Cupdate call cargo#update(<q-args>)
command! -nargs=* Csearch  call cargo#search(<q-args>)
command! -nargs=* Cpublish call cargo#publish(<q-args>)
command! -nargs=* Cinstall call cargo#install(<q-args>)
