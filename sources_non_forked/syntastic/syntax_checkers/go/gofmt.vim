"============================================================================
"File:        gofmt.vim
"Description: Check go syntax using 'gofmt -l'
"Maintainer:  Brandon Thomson <bt@brandonthomson.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" This syntax checker does not reformat your source code.
" Use a BufWritePre autocommand to that end:
"   autocmd FileType go autocmd BufWritePre <buffer> Fmt

if exists('g:loaded_syntastic_go_gofmt_checker')
    finish
endif
let g:loaded_syntastic_go_gofmt_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_go_gofmt_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '-l',
        \ 'tail_after': '> ' . syntastic#util#DevNull() })

    let errorformat = '%f:%l:%c: %m,%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'type': 'e'} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'go',
    \ 'name': 'gofmt'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
