"============================================================================
"File:        racket.vim
"Description: Syntax checking plugin for syntastic.vim
"Author:      Steve Bragg <steve at empresseffects dot com>
"
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_racket_racket_checker')
    finish
endif
let g:loaded_syntastic_racket_racket_checker=1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_racket_racket_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    " example of error message
    "eval-apply.rkt:460:30: the-empty-environment: unbound identifier in module
    "  in: the-empty-environment
    let errorformat = '%f:%l:%v: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })

    for e in loclist
        if has_key(e, 'col')
            let e['col'] += 1
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'racket',
    \ 'name': 'racket',
    \ 'enable': 'enable_racket_racket_checker' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
