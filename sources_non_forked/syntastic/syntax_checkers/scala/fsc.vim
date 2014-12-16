"============================================================================
"File:        fsc.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Gregor Uhlenheuer <kongo2002 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_scala_fsc_checker')
    finish
endif
let g:loaded_syntastic_scala_fsc_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_scala_fsc_GetLocList() dict
    call syntastic#log#deprecationWarn('scala_options', 'scala_fsc_args')

    " fsc has some serious problems with the
    " working directory changing after being started
    " that's why we better pass an absolute path
    let makeprg = self.makeprgBuild({
        \ 'args_after': '-Ystop-after:parser',
        \ 'fname': syntastic#util#shexpand('%:p') })

    let errorformat =
        \ '%E%f:%l: %trror: %m,' .
        \ '%Z%p^,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'scala',
    \ 'name': 'fsc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
