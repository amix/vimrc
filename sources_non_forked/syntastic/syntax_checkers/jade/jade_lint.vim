"============================================================================
"File:        jade_lint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Ben Parnell <benjaminparnell.94@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_jade_jade_lint_checker')
    finish
endif
let g:loaded_syntastic_jade_jade_lint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_jade_jade_lint_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-r inline' })

    let errorformat = '%f:%l:%c %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 2] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'jade',
    \ 'name': 'jade_lint',
    \ 'exec': 'jade-lint' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
