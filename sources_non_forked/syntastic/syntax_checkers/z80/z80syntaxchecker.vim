"============================================================================
"File:        z80syntaxchecker.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Romain Giot <giot.romain at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_z80_z80syntaxchecker_checker")
    finish
endif
let g:loaded_syntastic_z80_z80syntaxchecker_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_z80_z80syntaxchecker_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat =  '%f:%l %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'z80',
    \ 'name': 'z80syntaxchecker',
    \ 'exec': 'z80_syntax_checker.py'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
