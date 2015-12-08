"============================================================================
"File:        gotype.vim
"Description: Perform syntactic and semantic checking of Go code using 'gotype'
"Maintainer:  luz <ne.tetewi@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_go_gotype_checker')
    finish
endif
let g:loaded_syntastic_go_gotype_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_go_gotype_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args': (expand('%', 1) =~# '\m_test\.go$' ? '-a' : ''),
        \ 'fname': '.' })

    let errorformat =
        \ '%f:%l:%c: %m,' .
        \ '%-G%.%#'

    " gotype needs the full go package to test types properly. Just cwd to
    " the package for the same reasons specified in go.vim ("figuring out
    " the import path is fickle").

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'cwd': expand('%:p:h', 1),
        \ 'defaults': {'type': 'e'} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'go',
    \ 'name': 'gotype'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
