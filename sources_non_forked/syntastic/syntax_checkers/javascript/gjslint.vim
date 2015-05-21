"============================================================================
"File:        gjslint.vim
"Description: Javascript syntax checker - using gjslint
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_javascript_gjslint_checker")
    finish
endif
let g:loaded_syntastic_javascript_gjslint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_javascript_gjslint_GetLocList() dict
    call syntastic#log#deprecationWarn('javascript_gjslint_conf', 'javascript_gjslint_args')

    let makeprg = self.makeprgBuild({
        \ 'args_after': '--nosummary --unix_mode --nodebug_indentation --nobeep' })

    let errorformat =
        \ "%f:%l:(New Error -%\\?\%n) %m," .
        \ "%f:%l:(-%\\?%n) %m," .
        \ "%-G1 files checked," .
        \ " no errors found.," .
        \ "%-G%.%#"

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'gjslint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
