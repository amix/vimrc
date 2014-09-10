"============================================================================
"File:        haml_lint.vim
"Description: HAML style and syntax checker plugin for Syntastic
"Maintainer:  Shane da Silva <shane@dasilva.io>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_haml_haml_lint_checker")
    finish
endif
let g:loaded_syntastic_haml_haml_lint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_haml_haml_lint_GetLocList() dict
    let makeprg = self.makeprgBuild({})
    let errorformat = '%f:%l [%t] %m'
    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style'})
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haml',
    \ 'name': 'haml_lint',
    \ 'exec': 'haml-lint' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
