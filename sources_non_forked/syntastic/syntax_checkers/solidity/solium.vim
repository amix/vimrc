"============================================================================
"File:        solium.vim
"Description: Solidity syntax checker - using solium
"Maintainer:  Matthijs van den Bos <matthijs@vandenbos.org>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_solidity_solium_checker')
    finish
endif
let g:loaded_syntastic_solidity_solium_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_solidity_solium_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '-R gcc',
        \ 'fname_before': '--file'})

    let errorformat =
        \ '%f:%l:%c: %trror: %m,' .
        \ '%f:%l:%c: %tarning: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'solidity',
    \ 'name': 'solium'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
