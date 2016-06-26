"============================================================================
"File:        tern_lint.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_javascript_tern_lint_checker')
    finish
endif
let g:loaded_syntastic_javascript_tern_lint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_javascript_tern_lint_IsAvailable() dict
    return has('byte_offset') && executable(self.getExec())
endfunction

function! SyntaxCheckers_javascript_tern_lint_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat = '%f:%t:%l:%c:%n:%m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'tern_lint',
        \ 'returns': [0] })

    for e in loclist
        if get(e, 'col', 0) && get(e, 'nr', 0)
            let e['hl'] = '\%>' . (e['col'] - 1) . 'c\%<' . (e['nr'] + 1) . 'c'
        endif
        let e['nr'] = 0
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'tern_lint',
    \ 'exec': 'tern-lint' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
