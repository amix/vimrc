if exists('g:loaded_rust_vim_plugin_cargo')
    finish
endif
let g:loaded_rust_vim_plugin_cargo = 1
let s:save_cpo = &cpoptions
set cpoptions&vim

command! -nargs=+ Cargo call cargo#cmd(<q-args>)
command! -nargs=* Cbuild call cargo#build(<q-args>)
command! -nargs=* Ccheck call cargo#check(<q-args>)
command! -nargs=* Cclean call cargo#clean(<q-args>)
command! -nargs=* Cdoc call cargo#doc(<q-args>)
command! -nargs=+ Cnew call cargo#new(<q-args>)
command! -nargs=* Cinit call cargo#init(<q-args>)
command! -nargs=* Crun call cargo#run(<q-args>)
command! -nargs=* Ctest call cargo#test(<q-args>)
command! -nargs=* Cbench call cargo#bench(<q-args>)
command! -nargs=* Cupdate call cargo#update(<q-args>)
command! -nargs=* Csearch  call cargo#search(<q-args>)
command! -nargs=* Cpublish call cargo#publish(<q-args>)
command! -nargs=* Cinstall call cargo#install(<q-args>)
command! -nargs=* Cruntarget call cargo#runtarget(<q-args>)

let &cpoptions = s:save_cpo
unlet s:save_cpo

" vim: set et sw=4 sts=4 ts=8:
