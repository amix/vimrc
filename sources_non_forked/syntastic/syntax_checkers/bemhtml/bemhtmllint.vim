"============================================================================
"File:        bemhtmllint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Sergej Tatarincev <s.tatarincev at yandex.ua>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_bemhtml_bemhtmllint_checker")
    finish
endif

let g:loaded_syntastic_bemhtml_bemhtmllint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function SyntaxCheckers_bemhtml_bemhtmllint_GetLocList() dict
    let makeprg = self.makeprgBuild({})
    let errorformat = '%f:%l:%c: %m'
    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'bemhtml',
    \ 'name': 'bemhtmllint',
    \ 'exec': 'bemhtml-lint' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
