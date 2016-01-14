"============================================================================
"File:        dockerfile_lint.vim
"Description: Syntax checking plugin for syntastic.vim using dockerfile-lint
"             (https://github.com/projectatomic/dockerfile-lint).
"Maintainer:  Tim Carry <tim at pixelastic dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_dockerfile_dockerfile_lint_checker')
    finish
endif
let g:loaded_syntastic_dockerfile_dockerfile_lint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_dockerfile_dockerfile_lint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '-j',
        \ 'fname_before': '-f' })

    let errorformat = '%t:%n:%l:%m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'dockerfile_lint',
        \ 'defaults': {'bufnr': bufnr('')},
        \ 'returns': [0, 1] })

    for e in loclist
        if e['nr']
            let e['subtype'] = 'Style'
        endif
        call remove(e, 'nr')
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'dockerfile',
    \ 'name': 'dockerfile_lint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
