"============================================================================
"File:        rubylint.vim
"Description: Checks Ruby source code using ruby-lint
"Maintainer:  Yorick Peterse <yorickpeterse@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_ruby_rubylint_checker")
    finish
endif

let g:loaded_syntastic_ruby_rubylint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_ruby_rubylint_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args': 'analyze --presenter=syntastic' })

    let errorformat = '%f:%t:%l:%c: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ruby',
    \ 'name': 'rubylint',
    \ 'exec': 'ruby-lint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
