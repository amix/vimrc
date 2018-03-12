"============================================================================
"File:        solhint.vim
"Description: Solidity syntax checker - using solhint
"Maintainer:  Brett Sun <qisheng.brett.sun@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_solidity_solhint_checker')
    finish
endif
let g:loaded_syntastic_solidity_solhint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_solidity_solhint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '-f compact' })

    let errorformat =
        \ '%E%f: line %l\, col %c\, Error - %m,' .
        \ '%W%f: line %l\, col %c\, Warning - %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'solidity',
    \ 'name': 'solhint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
