"============================================================================
"File:        html.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_html_htmlhint_checker')
    finish
endif
let g:loaded_syntastic_html_htmlhint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_html_htmlhint_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(), [0, 9, 13])
endfunction

function! SyntaxCheckers_html_htmlhint_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_before': '--format unix' })

    let errorformat = '%f:%l:%c: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'html',
    \ 'name': 'htmlhint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
