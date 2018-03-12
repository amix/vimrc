"============================================================================
"File:        stylelint.vim
"Description: Syntax checking plugin for syntastic using `stylelint`
"             (https://github.com/stylelint/stylelint).
"Maintainer:  Tim Carry <tim at pixelastic dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_css_stylelint_checker')
    finish
endif
let g:loaded_syntastic_css_stylelint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

let s:args_after = {
    \ 'css':  '-f json',
    \ 'html': '-f json',
    \ 'less': '-f json -s less',
    \ 'scss': '-f json -s scss' }

function! SyntaxCheckers_css_stylelint_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': get(s:args_after, self.getFiletype(), '') })

    let errorformat = '%t:%f:%l:%c:%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'preprocess': 'stylelint',
        \ 'returns': [0, 1, 2] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'css',
    \ 'name': 'stylelint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:

