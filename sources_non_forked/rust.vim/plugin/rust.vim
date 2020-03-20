" Vim syntastic plugin helper
" Language:     Rust
" Maintainer:   Andrew Gallant <jamslam@gmail.com>

if exists('g:loaded_rust_vim')
    finish
endif
let g:loaded_rust_vim = 1
let s:save_cpo = &cpoptions
set cpoptions&vim

" This is to let Syntastic know about the Rust filetype.
" It enables tab completion for the 'SyntasticInfo' command.
" (This does not actually register the syntax checker.)
if exists('g:syntastic_extra_filetypes')
    call add(g:syntastic_extra_filetypes, 'rust')
else
    let g:syntastic_extra_filetypes = ['rust']
endif

if !exists('g:syntastic_rust_checkers')
    let g:syntastic_rust_checkers = ['cargo']
endif

let &cpoptions = s:save_cpo
unlet s:save_cpo

" vim: set et sw=4 sts=4 ts=8:
