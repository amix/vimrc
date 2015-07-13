"============================================================================
"File:        standard.vim
"Description: JavaScript syntax checker - using standard
"Maintainer:  LCD 47 <lcd047@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_javascript_standard_checker')
    finish
endif
let g:loaded_syntastic_javascript_standard_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_javascript_standard_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(), [2, 6, 1])
endfunction

function! SyntaxCheckers_javascript_standard_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args': '-v' })

    let errorformat = '  %f:%l:%c: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'defaults': {'type': 'W'},
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'standard'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
