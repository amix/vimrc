"============================================================================
"File:        jsl.vim
"Description: Javascript syntax checker - using jsl
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_javascript_jsl_checker")
    finish
endif
let g:loaded_syntastic_javascript_jsl_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_javascript_jsl_GetLocList() dict
    call syntastic#log#deprecationWarn('javascript_jsl_conf', 'javascript_jsl_args',
        \ "'-conf ' . syntastic#util#shexpand(OLD_VAR)")

    let makeprg = self.makeprgBuild({
        \ 'args_after': '-nologo -nofilelisting -nosummary -nocontext -process' })

    let errorformat =
        \ '%W%f(%l): lint warning: %m,'.
        \ '%-Z%p^,'.
        \ '%W%f(%l): warning: %m,'.
        \ '%-Z%p^,'.
        \ '%E%f(%l): SyntaxError: %m,'.
        \ '%-Z%p^,'.
        \ '%-G'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'jsl'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
