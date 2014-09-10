"============================================================================
"File:        css.vim
"Description: Syntax checking plugin for syntastic.vim using `csslint` CLI tool (http://csslint.net).
"Maintainer:  Ory Band <oryband at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================
"
" Specify additional options to csslint with this option. e.g. to disable
" warnings:
"
"   let g:syntastic_csslint_options = '--warnings=none'

if exists('g:loaded_syntastic_css_csslint_checker')
    finish
endif
let g:loaded_syntastic_css_csslint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_css_csslint_GetLocList() dict
    call syntastic#log#deprecationWarn('csslint_options', 'css_csslint_args')

    let makeprg = self.makeprgBuild({ 'args_after': '--format=compact' })

    " Print CSS Lint's error/warning messages from compact format. Ignores blank lines.
    let errorformat =
        \ '%-G,' .
        \ '%-G%f: lint free!,' .
        \ '%f: line %l\, col %c\, %trror - %m,' .
        \ '%f: line %l\, col %c\, %tarning - %m,'.
        \ '%f: line %l\, col %c\, %m,'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")} })

endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'css',
    \ 'name': 'csslint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
