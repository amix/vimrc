"============================================================================
"File:        yacc.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_yacc_bison_checker')
    finish
endif
let g:loaded_syntastic_yacc_bison_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_yacc_bison_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': syntastic#c#NullOutput() })

    let errorformat =
        \ '%E%f:%l%.%v-%.%\{-}: %trror: %m,' .
        \ '%E%f:%l%.%v: %trror: %m,' .
        \ '%W%f:%l%.%v-%.%\{-}: %tarning: %m,' .
        \ '%W%f:%l%.%v: %tarning: %m,' .
        \ '%I%f:%l%.%v-%.%\{-}: %\s%\+%m,' .
        \ '%I%f:%l%.%v: %\s%\+%m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })

    let last_type = 'E'
    for e in loclist
        if e['type'] ==? 'I'
            let e['type'] = last_type
        endif
        let last_type = e['type']
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'yacc',
    \ 'name': 'bison'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
