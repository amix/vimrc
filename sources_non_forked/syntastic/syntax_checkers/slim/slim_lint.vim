"============================================================================
"File:        slim_lint.vim
"Description: Slim style and syntax checker plugin for Syntastic
"Maintainer:  Vasily Kolesnikov <re.vkolesnikov@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_slim_slim_lint_checker')
    finish
endif
let g:loaded_syntastic_slim_slim_lint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_slim_slim_lint_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat = '%f:%l [%t] %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style'})
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'slim',
    \ 'name': 'slim_lint',
    \ 'exec': 'slim-lint' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
