"============================================================================
"File:        bashate.vim
"Description: Bash script style checking plugin for syntastic
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_sh_bashate_checker')
    finish
endif
let g:loaded_syntastic_sh_bashate_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_sh_bashate_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat =
        \ '%A%\s%#[%t] E%n: %m,' .
        \ '%EE%n: %m,' .
        \ '%Z - %f%\s%\+: L%l,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'returns': [0, 1] })

    for e in loclist
        let e['text'] = substitute(e['text'], "\\m: '.*", '', '')
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'sh',
    \ 'name': 'bashate' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
