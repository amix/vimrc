"============================================================================
"File:        w3.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_html_w3_checker")
    finish
endif
let g:loaded_syntastic_html_w3_checker = 1

if !exists('g:syntastic_html_w3_api')
    let g:syntastic_html_w3_api = 'http://validator.w3.org/check'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_html_w3_GetLocList() dict
    let makeprg = self.getExecEscaped() . ' -q -s -F output=json ' .
        \ '-F uploaded_file=@' . syntastic#util#shexpand('%:p') . '\;type=text/html ' .
        \ g:syntastic_html_w3_api

    let errorformat =
        \ '%A %\+{,' .
        \ '%C %\+"lastLine": %l\,%\?,' .
        \ '%C %\+"lastColumn": %c\,%\?,' .
        \ '%C %\+"message": "%m"\,%\?,' .
        \ '%C %\+"type": "%trror"\,%\?,' .
        \ '%-G %\+"type": "%tnfo"\,%\?,' .
        \ '%C %\+"subtype": "%tarning"\,%\?,' .
        \ '%Z %\+}\,,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")},
        \ 'returns': [0] })

    for e in loclist
        let e['text'] = substitute(e['text'], '\m\\\([\"]\)', '\1', 'g')
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'html',
    \ 'name': 'w3',
    \ 'exec': 'curl' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
