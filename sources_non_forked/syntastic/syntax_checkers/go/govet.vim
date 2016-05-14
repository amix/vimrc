"============================================================================
"File:        govet.vim
"Description: Perform static analysis of Go code with the vet tool
"Maintainer:  Kamil Kisiel <kamil@kamilkisiel.net>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_go_govet_checker')
    finish
endif
let g:loaded_syntastic_go_govet_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_go_govet_GetLocList() dict
    let makeprg = self.getExecEscaped() . ' vet'

    let errorformat =
        \ '%Evet: %.%\+: %f:%l:%c: %m,' .
        \ '%W%f:%l: %m,' .
        \ '%-G%.%#'

    " The go compiler needs to either be run with an import path as an
    " argument or directly from the package directory. Since figuring out
    " the proper import path is fickle, just cwd to the package.

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'cwd': expand('%:p:h', 1),
        \ 'defaults': {'type': 'w'} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'go',
    \ 'name': 'govet',
    \ 'exec': 'go' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
