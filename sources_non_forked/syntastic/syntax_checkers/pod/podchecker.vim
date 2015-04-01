"============================================================================
"File:        podchecker.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_pod_podchecker_checker")
    finish
endif
let g:loaded_syntastic_pod_podchecker_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_pod_podchecker_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat =
        \ '%W%[%#]%[%#]%[%#] WARNING: %m at line %l in file %f,' .
        \ '%W%[%#]%[%#]%[%#] WARNING: %m at line EOF in file %f,' .
        \ '%E%[%#]%[%#]%[%#] ERROR: %m at line %l in file %f,' .
        \ '%E%[%#]%[%#]%[%#] ERROR: %m at line EOF in file %f'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1, 2] })

    for e in loclist
        if e['valid'] && e['lnum'] == 0
            let e['lnum'] = str2nr(matchstr(e['text'], '\m\<line \zs\d\+\ze'))
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'pod',
    \ 'name': 'podchecker'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
